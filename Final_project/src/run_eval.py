#!/usr/bin/env python3
"""
Tiered LLM evaluation: temporal masking + timeline / era prediction.

Usage (from Final_project directory):
  pip install -r requirements.txt
  cp .env.example .env
  ollama pull llama3.2
  python -m src.run_eval --tier all \\
    --model ollama:llama3.2 --model ollama:mistral \\
    --timebank-dir data/corpora/samples/timebank \\
    --matres-tsv data/corpora/samples/matres/matres_mini.tsv \\
    --matres-tml-dir data/corpora/samples/matres

Corpora: see data/corpora/README.txt (NYT, COHA, TimeBank, MATRES).
Synthetic: data/synthetic/README.txt and  python -m src.synthetic_generator
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional

try:
    from dotenv import load_dotenv
except ImportError:
    load_dotenv = None  # type: ignore

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from src.corpus_loaders import (
    load_coha_txt_dir,
    load_matres_tsv,
    load_nyt_or_jsonl,
    load_te3_unit_csv,
    load_synthetic_json,
    load_timebank_dir,
    load_tier_json,
    merge_samples,
)
from src.llm_client import LLMError, complete_json_task
from src.mask_temporal import mask_temporal
from src.metrics import (
    mean_reciprocal_rank_match,
    pairwise_order_accuracy,
    parse_year_range_from_json,
    summarize_by_model_and_tier,
    summarize_tier_results,
    year_range_overlap,
)
from src.model_spec import ModelSpec, parse_model_args

if load_dotenv is not None:
    load_dotenv(ROOT / ".env")

SYSTEM_PROMPT = """You are a careful research assistant evaluating temporal reasoning.
You must respond with ONLY valid JSON (no markdown, no commentary) matching this schema:
{
  "ordered_events": ["string", ...],
  "approximate_era": "short free-text label for the historical or setting context",
  "estimated_year_range": [start_year, end_year] or null if not inferable
}
Rules:
- ordered_events: key events in true chronological order (earliest first).
- Use short English phrases; do not copy the entire document.
- If the text is fictional or non-dated, set estimated_year_range to null and explain in approximate_era.
"""


def build_user_prompt(masked_text: str) -> str:
    return (
        "Below is a document with explicit dates and years removed or replaced by "
        "placeholders like [YEAR]. Infer a coherent timeline and era.\n\n"
        "--- DOCUMENT ---\n"
        f"{masked_text}\n"
        "--- END ---\n"
    )


def eval_one(
    sample: Dict[str, Any],
    aggressive_mask: bool,
    model_spec: ModelSpec,
) -> Dict[str, Any]:
    if sample.get("skip_reason"):
        return {
            "id": sample.get("id"),
            "tier": sample.get("tier"),
            "corpus": sample.get("corpus"),
            "model": model_spec.key(),
            "skipped": True,
            "skip_reason": sample["skip_reason"],
        }

    raw_text = sample["text"]
    masked = mask_temporal(raw_text, aggressive=aggressive_mask)
    gold_events = sample.get("gold_events_chronological") or []
    gold_years = sample.get("gold_year_range")
    gold_tuple = None
    if isinstance(gold_years, list) and len(gold_years) == 2:
        try:
            a, b = gold_years[0], gold_years[1]
            if a is not None and b is not None:
                gold_tuple = (int(a), int(b))
        except (TypeError, ValueError):
            gold_tuple = None

    user = build_user_prompt(masked)
    try:
        out = complete_json_task(SYSTEM_PROMPT, user, model_spec=model_spec)
    except LLMError as e:
        return {
            "id": sample.get("id"),
            "tier": sample.get("tier"),
            "corpus": sample.get("corpus"),
            "model": model_spec.key(),
            "error": str(e),
            "masked_document": masked,
        }

    pred_events = out.get("ordered_events") or []
    if not isinstance(pred_events, list):
        pred_events = []
    pred_events = [str(x) for x in pred_events]

    pred_range = parse_year_range_from_json(out.get("estimated_year_range"))

    if not gold_events:
        return {
            "id": sample.get("id"),
            "tier": sample.get("tier"),
            "corpus": sample.get("corpus"),
            "model": model_spec.key(),
            "masked_document": masked,
            "gold_events_chronological": gold_events,
            "predicted_ordered_events": pred_events,
            "gold_year_range": gold_tuple,
            "predicted_year_range": pred_range,
            "pairwise_order_accuracy": None,
            "pairwise_order_correct": 0,
            "pairwise_order_total": 0,
            "mean_best_match": None,
            "year_overlap": year_range_overlap(pred_range, gold_tuple) if gold_tuple else None,
            "approximate_era_model": out.get("approximate_era"),
            "gold_era_label": sample.get("gold_era_label"),
            "raw_model_json": out,
            "note": "No gold events for this document; order metrics are null.",
        }

    poa, good, total = pairwise_order_accuracy(gold_events, pred_events)
    mbm = mean_reciprocal_rank_match(gold_events, pred_events)
    yo = year_range_overlap(pred_range, gold_tuple)

    return {
        "id": sample.get("id"),
        "tier": sample.get("tier"),
        "corpus": sample.get("corpus"),
        "model": model_spec.key(),
        "masked_document": masked,
        "gold_events_chronological": gold_events,
        "predicted_ordered_events": pred_events,
        "gold_year_range": gold_tuple,
        "predicted_year_range": pred_range,
        "pairwise_order_accuracy": poa,
        "pairwise_order_correct": good,
        "pairwise_order_total": total,
        "mean_best_match": mbm,
        "year_overlap": yo,
        "approximate_era_model": out.get("approximate_era"),
        "gold_era_label": sample.get("gold_era_label"),
        "raw_model_json": out,
    }


def collect_samples(args: argparse.Namespace) -> List[Dict[str, Any]]:
    chunks: List[List[Dict[str, Any]]] = []

    # Fixed evaluation design (per project writeup):
    # - MATRES + TempEval/TimeBank-style corpora are "easy"
    # - Synthetic generator outputs are "hard"
    # - Always evaluate on: 25 MATRES, 25 TempEval, 50 Synthetic (when provided)
    FIX_MATRES = 25
    FIX_TEMPEVAL = 25
    FIX_SYNTHETIC = 50

    if args.nyt_jsonl:
        chunks.append(load_nyt_or_jsonl(Path(args.nyt_jsonl)))
    if args.coha_dir:
        chunks.append(load_coha_txt_dir(Path(args.coha_dir), limit=args.corpus_limit))
    if args.timebank_dir:
        chunks.append(load_timebank_dir(Path(args.timebank_dir), limit=FIX_TEMPEVAL))
    if args.matres_tsv:
        chunks.append(
            load_matres_tsv(
                Path(args.matres_tsv),
                tml_dir=Path(args.matres_tml_dir) if args.matres_tml_dir else None,
                limit_docs=FIX_MATRES,
            )
        )
    if args.te3_unit_csv:
        chunks.append(load_te3_unit_csv(Path(args.te3_unit_csv), limit_docs=FIX_TEMPEVAL))
    if args.synthetic_json:
        chunks.append(load_synthetic_json(Path(args.synthetic_json), limit=FIX_SYNTHETIC))
    if not chunks:
        # Fallback for quick smoke tests without external corpora.
        chunks.append(load_tier_json(Path(args.data)))

    merged = merge_samples(*chunks)

    # Enforce the fixed corpus composition when those sources are present.
    have_any_fixed = any(
        [
            bool(args.timebank_dir),
            bool(args.te3_unit_csv),
            bool(args.matres_tsv),
            bool(args.synthetic_json),
        ]
    )
    if not have_any_fixed:
        return merged

    matres = [s for s in merged if s.get("corpus") == "matres"][:FIX_MATRES]
    tempeval = [s for s in merged if s.get("corpus") == "tempeval"][:FIX_TEMPEVAL]
    synthetic = [s for s in merged if s.get("corpus") == "synthetic"][:FIX_SYNTHETIC]

    # Force tiers to match the new scheme.
    for s in matres:
        s["tier"] = "easy"
    for s in tempeval:
        s["tier"] = "easy"
    for s in synthetic:
        s["tier"] = "hard"

    return matres + tempeval + synthetic


def main() -> None:
    ap = argparse.ArgumentParser(description="Tiered temporal reasoning evaluation for LLMs.")
    ap.add_argument(
        "--data",
        type=Path,
        default=ROOT / "data" / "tier_samples.json",
        help="Bundled tier JSON (easy/hard hand-authored items).",
    )
    ap.add_argument(
        "--no-tier-samples",
        action="store_true",
        help="Exclude data/tier_samples.json when using external corpora only.",
    )
    ap.add_argument("--nyt-jsonl", type=str, default="", help="NYT-style JSONL manifest (see corpora README).")
    ap.add_argument("--coha-dir", type=str, default="", help="Directory of plain .txt COHA exports.")
    ap.add_argument("--timebank-dir", type=str, default="", help="Directory of TimeML .tml files (TimeBank/TempEval).")
    ap.add_argument("--matres-tsv", type=str, default="", help="MATRES TSV (docid, verb1, verb2, eiid1, eiid2, relation).")
    ap.add_argument(
        "--te3-unit-csv",
        type=str,
        default="",
        help="TempEval3 / TimeBank-Dense unit CSV (e.g., TBDense_all_new.csv). No gold order.",
    )
    ap.add_argument(
        "--matres-tml-dir",
        type=str,
        default="",
        help="TempEval3-style folder containing {docid}.tml for MATRES text.",
    )
    ap.add_argument(
        "--synthetic-json",
        type=str,
        default="",
        help="JSON array from src.synthetic_generator (synthetic hard-tier items).",
    )
    ap.add_argument(
        "--corpus-limit",
        type=int,
        default=None,
        metavar="N",
        help="Max documents per external corpus (default: all).",
    )
    ap.add_argument(
        "--tier",
        choices=["easy", "hard", "all"],
        default="all",
        help="Which tier to run.",
    )
    ap.add_argument(
        "--aggressive-mask",
        action="store_true",
        help="Also mask phrases like 'last Tuesday'.",
    )
    ap.add_argument(
        "--model",
        action="append",
        dest="models",
        default=None,
        help="Repeatable. Example: --model ollama:llama3.2 --model groq:llama-3.1-8b-instant",
    )
    ap.add_argument(
        "--out",
        type=Path,
        default=ROOT / "outputs" / "latest_run.json",
        help="Write full JSON results here.",
    )
    ap.add_argument(
        "--dry-run",
        action="store_true",
        help="Only load data and print masked documents (no LLM calls).",
    )
    args = ap.parse_args()

    model_specs: List[ModelSpec]
    if args.models:
        model_specs = parse_model_args(args.models)
    else:
        model_specs = [ModelSpec.from_env()]

    samples = collect_samples(args)
    if args.tier != "all":
        samples = [s for s in samples if s.get("tier") == args.tier]

    if args.dry_run:
        for s in samples:
            if s.get("skip_reason"):
                print(f"\n=== {s.get('id')} SKIP: {s['skip_reason']}\n")
                continue
            masked = mask_temporal(s["text"], aggressive=args.aggressive_mask)
            print(f"\n=== {s.get('id')} ({s.get('tier')}) [{s.get('corpus')}] ===\n{masked}\n")
        return

    results: List[Dict[str, Any]] = []
    for spec in model_specs:
        for s in samples:
            print(f"{spec.key()} | {s.get('id')} ({s.get('tier')}) ...", flush=True)
            results.append(eval_one(s, aggressive_mask=args.aggressive_mask, model_spec=spec))

    scored = [r for r in results if "error" not in r and not r.get("skipped")]
    summary_all = summarize_tier_results(scored)
    summary_models = summarize_by_model_and_tier(scored)
    report = {
        "models": [m.key() for m in model_specs],
        "summary_by_tier": summary_all,
        "summary_by_model_and_tier": summary_models,
        "results": results,
    }

    args.out.parent.mkdir(parents=True, exist_ok=True)
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, ensure_ascii=False)

    print(json.dumps({"summary_by_tier": summary_all, "by_model": summary_models}, indent=2))
    print(f"\nWrote {args.out}")


if __name__ == "__main__":
    main()
