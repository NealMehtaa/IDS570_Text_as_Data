#!/usr/bin/env python3
"""
Generate synthetic evaluation documents with a **generator** LLM (not the models
you test). Uses few-shot seeds + JSON-only output with embedded gold timelines.

Example:
  python -m src.synthetic_generator --n 5 --out data/synthetic/generated.json \\
    --generator ollama:mistral

See data/synthetic/README.txt.
"""

from __future__ import annotations

import argparse
import json
import sys
import time
import uuid
from pathlib import Path
from typing import Any, Dict, List, Optional

try:
    from dotenv import load_dotenv
except ImportError:
    load_dotenv = None  # type: ignore

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from src.llm_client import LLMError, complete_json_task
from src.model_spec import ModelSpec, parse_model_args

if load_dotenv is not None:
    load_dotenv(ROOT / ".env")

GENERATOR_SYSTEM = """You write **synthetic** English text for NLP research on temporal reasoning.
Respond with ONLY valid JSON (no markdown, no commentary).

Required schema:
{
  "id": "string, unique id like synthetic_gen_001",
  "text": "one or two short paragraphs, about 120–240 words total",
  "gold_events_chronological": ["short label 1", "label 2", ...],
  "gold_era_label": "brief setting description",
  "gold_year_range": [start_year, end_year] or null if no real-world calendar applies,
  "tier": "hard",
  "notes": "optional short note"
}

Content rules:
- gold_events_chronological must be the TRUE chronological order (earliest first) and must be recoverable from the narrative using order and discourse cues.
- Embed events in running prose; do NOT present the gold list as a numbered outline inside `text`.
- Avoid explicit calendar years and full dates in `text` (phrases like "the following spring" or "last quarter" are fine).
- Avoid easily recognizable real public figures, iconic battles, or copy-pasted news headlines. Prefer fictional place names or generic institutions.
- Vary themes across calls: sometimes fictional polities, sometimes abstract organizations or local communities.
"""


def _load_seeds(path: Path) -> List[Dict[str, Any]]:
    with open(path, encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, list):
        raise ValueError("Few-shot file must be a JSON array.")
    return data


def _format_few_shot(seeds: List[Dict[str, Any]]) -> str:
    parts = []
    for s in seeds:
        parts.append(json.dumps(s, ensure_ascii=False, indent=2))
    return "\n\n--- EXAMPLE (do not copy; imitate structure and style) ---\n\n".join(parts)


def _validate_item(obj: Dict[str, Any]) -> None:
    for k in ("text", "gold_events_chronological", "gold_era_label"):
        if k not in obj or obj[k] in (None, "", []):
            raise ValueError(f"Missing or empty field: {k}")
    if not isinstance(obj["gold_events_chronological"], list) or len(obj["gold_events_chronological"]) < 3:
        raise ValueError("Need at least 3 gold events.")
    obj.setdefault("tier", "hard")
    obj.setdefault("corpus", "synthetic")
    if "id" not in obj or not str(obj["id"]).strip():
        obj["id"] = f"synthetic_gen_{uuid.uuid4().hex[:10]}"


def generate_one(
    *,
    generator_spec: ModelSpec,
    seeds: List[Dict[str, Any]],
    index: int,
    total: int,
    seeds_path: Path,
) -> Dict[str, Any]:
    few = _format_few_shot(seeds)
    user = (
        f"You will produce document {index + 1} of {total} for a benchmark.\n"
        "Study the examples below, then write a **new** passage that is not a paraphrase of any example.\n\n"
        f"{few}\n\n"
        "Now output a single JSON object for your new document following the schema in the system message."
    )
    out = complete_json_task(GENERATOR_SYSTEM, user, model_spec=generator_spec)
    _validate_item(out)
    out["corpus"] = "synthetic"
    out["tier"] = out.get("tier") or "hard"
    out["provenance"] = {
        "generator": generator_spec.key(),
        "few_shot_file": str(seeds_path),
    }
    return out


def _load_existing_out(path: Path) -> List[Dict[str, Any]]:
    if not path.is_file():
        return []
    with open(path, encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, list):
        raise ValueError(f"Expected JSON array in {path}, got {type(data).__name__}.")
    out: List[Dict[str, Any]] = []
    for obj in data:
        if isinstance(obj, dict):
            out.append(obj)
    return out


def _append_failure_log(path: Optional[Path], msg: str) -> None:
    if path is None:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "a", encoding="utf-8") as f:
        f.write(msg.rstrip() + "\n")


def main() -> None:
    ap = argparse.ArgumentParser(description="Generate synthetic timeline benchmark documents.")
    ap.add_argument("--n", type=int, default=3, help="How many new documents to generate.")
    ap.add_argument(
        "--out",
        type=Path,
        default=ROOT / "data" / "synthetic" / "generated.json",
        help="Write a JSON array of samples (tier_samples schema).",
    )
    ap.add_argument(
        "--seeds",
        type=Path,
        default=ROOT / "data" / "synthetic" / "few_shot_seeds.json",
        help="Few-shot JSON array (see default file).",
    )
    ap.add_argument(
        "--generator",
        type=str,
        default="",
        help="Generator model backend:id, e.g. ollama:mistral. Overrides GENERATOR_* env.",
    )
    ap.add_argument(
        "--resume",
        action="store_true",
        help="If --out already exists, load it and continue until reaching --n total items.",
    )
    ap.add_argument(
        "--max-attempts",
        type=int,
        default=0,
        help="Upper bound on total generation attempts (0 = unlimited).",
    )
    ap.add_argument(
        "--max-failures",
        type=int,
        default=0,
        help="Stop after this many failures (0 = unlimited).",
    )
    ap.add_argument(
        "--sleep-seconds",
        type=float,
        default=0.0,
        help="Sleep this many seconds after each attempt (helps avoid overload).",
    )
    ap.add_argument(
        "--failure-log",
        type=Path,
        default=None,
        help="Append failures to this text file (optional).",
    )
    args = ap.parse_args()

    if args.generator.strip():
        generator_spec = parse_model_args([args.generator])[0]
    else:
        generator_spec = ModelSpec.from_env_generator()

    seeds = _load_seeds(args.seeds)
    results: List[Dict[str, Any]] = _load_existing_out(args.out) if args.resume else []
    seen_ids = {str(x.get("id")) for x in results if isinstance(x, dict) and x.get("id")}

    target = int(args.n)
    if target < 1:
        raise ValueError("--n must be >= 1")

    attempt = 0
    failures = 0
    start = len(results)
    if start:
        print(f"Resuming: already have {start} items in {args.out}", flush=True)

    while len(results) < target:
        attempt += 1
        if args.max_attempts and attempt > int(args.max_attempts):
            raise RuntimeError(
                f"Stopped: reached --max-attempts={args.max_attempts} with {len(results)}/{target} items."
            )
        if args.max_failures and failures >= int(args.max_failures):
            raise RuntimeError(
                f"Stopped: reached --max-failures={args.max_failures} with {len(results)}/{target} items."
            )

        idx = len(results)
        print(f"Generating {idx + 1}/{target} via {generator_spec.key()} (attempt {attempt}) ...", flush=True)

        try:
            item = generate_one(
                generator_spec=generator_spec,
                seeds=seeds,
                index=idx,
                total=target,
                seeds_path=args.seeds.resolve(),
            )
            sid = str(item.get("id") or "").strip()
            if not sid or sid in seen_ids:
                item["id"] = f"synthetic_gen_{uuid.uuid4().hex[:10]}"
                sid = str(item["id"])
            seen_ids.add(sid)
            results.append(item)

            # Persist progress so long runs are resumable.
            args.out.parent.mkdir(parents=True, exist_ok=True)
            with open(args.out, "w", encoding="utf-8") as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
        except (LLMError, ValueError) as e:
            failures += 1
            msg = f"FAILED attempt {attempt} (kept {len(results)}/{target}): {e}"
            print(msg, flush=True)
            _append_failure_log(args.failure_log, msg)
            # Keep going; one bad JSON shouldn't kill a 1000-item run.

        if args.sleep_seconds and float(args.sleep_seconds) > 0:
            time.sleep(float(args.sleep_seconds))

    print(f"Wrote {len(results)} items to {args.out}")


if __name__ == "__main__":
    main()
