#!/usr/bin/env python3
"""
Visualizations for run_eval JSON outputs (matplotlib only).

Usage (from Final_project directory):
  python -m src.plot_results --input outputs/latest_run.json
  python -m src.plot_results --input outputs/full_3_ollama.json --out-dir report/figures

Writes PNG (and optional PDF) suitable for inclusion in a LaTeX report.
Requires: matplotlib (and numpy, pulled in by matplotlib).
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any, Dict, List, Tuple

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


def load_report(path: Path) -> Dict[str, Any]:
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def _save_current_figure(out: Path, pdf: bool) -> None:
    plt.tight_layout()
    plt.savefig(out, dpi=160, bbox_inches="tight")
    if pdf:
        plt.savefig(out.with_suffix(".pdf"), bbox_inches="tight")
    plt.close()


def summary_to_matrix(
    by_model: Dict[str, Dict[str, Any]],
    field: str,
    tiers: Tuple[str, ...] = ("easy", "hard"),
) -> Tuple[List[str], np.ndarray]:
    """Return (model_labels row order, matrix shape [n_models, n_tiers]) with NaN for missing."""
    models = sorted(by_model.keys())
    mat = np.full((len(models), len(tiers)), np.nan, dtype=float)
    for i, mk in enumerate(models):
        tier_stats = by_model[mk]
        for j, t in enumerate(tiers):
            cell = tier_stats.get(t) or {}
            v = cell.get(field)
            if v is not None:
                mat[i, j] = float(v)
    short_names = [m.replace("ollama:", "").replace("groq:", "") for m in models]
    return short_names, mat


def plot_grouped_bars_matrix(
    models: List[str],
    mat: np.ndarray,
    tier_labels: Tuple[str, ...],
    ylabel: str,
    title: str,
    out: Path,
    pdf: bool,
) -> None:
    n_tiers = len(tier_labels)
    n_models = len(models)
    x = np.arange(n_tiers, dtype=float)
    width = min(0.8 / max(n_models, 1), 0.25)
    plt.figure(figsize=(9, 5))
    for i in range(n_models):
        offset = (i - (n_models - 1) / 2) * width
        vals = mat[i]
        plt.bar(x + offset, vals, width=width * 0.95, label=models[i])
    plt.xticks(x, list(tier_labels))
    plt.ylabel(ylabel)
    plt.xlabel("Difficulty tier")
    plt.ylim(0, 1.05)
    plt.title(title)
    plt.legend(title="Model", bbox_to_anchor=(1.02, 1), loc="upper left")
    _save_current_figure(out, pdf)


def plot_heatmap(
    models: List[str],
    mat: np.ndarray,
    tier_labels: Tuple[str, ...],
    title: str,
    out: Path,
    pdf: bool,
) -> None:
    plt.figure(figsize=(7, max(3, 0.45 * len(models) + 1)))
    im = plt.imshow(mat, aspect="auto", cmap="YlGnBu", vmin=0, vmax=1)
    plt.colorbar(im, fraction=0.046, pad=0.04, label=title)
    plt.yticks(range(len(models)), models)
    plt.xticks(range(len(tier_labels)), list(tier_labels))
    plt.xlabel("Tier")
    plt.ylabel("Model")
    for i in range(mat.shape[0]):
        for j in range(mat.shape[1]):
            v = mat[i, j]
            if not math.isnan(v):
                plt.text(j, i, f"{v:.2f}", ha="center", va="center", color="black", fontsize=9)
    plt.title(title)
    _save_current_figure(out, pdf)


def plot_scatter_per_doc(results: List[Dict[str, Any]], out: Path, pdf: bool) -> None:
    xs: List[float] = []
    ys: List[float] = []
    tiers: List[str] = []
    models: List[str] = []
    for r in results:
        if r.get("error") or r.get("skipped"):
            continue
        poa = r.get("pairwise_order_accuracy")
        mbm = r.get("mean_best_match")
        if poa is None or mbm is None:
            continue
        xs.append(float(mbm))
        ys.append(float(poa))
        tiers.append(str(r.get("tier", "")))
        models.append(str(r.get("model", "")).replace("ollama:", "").replace("groq:", ""))

    if not xs:
        return

    plt.figure(figsize=(7, 5))
    tier_set = sorted(set(tiers))
    markers = ["o", "s", "^", "D", "P", "X"]
    model_set = sorted(set(models))
    for ti, tier in enumerate(tier_set):
        for mi, model in enumerate(model_set):
            px = [x for x, t, m in zip(xs, tiers, models) if t == tier and m == model]
            py = [y for y, t, m in zip(ys, tiers, models) if t == tier and m == model]
            if not px:
                continue
            plt.scatter(
                px,
                py,
                s=55,
                alpha=0.85,
                marker=markers[(ti + mi) % len(markers)],
                label=f"{tier} / {model}",
            )
    plt.xlabel("Mean best string match (gold ↔ predicted events)")
    plt.ylabel("Pairwise order accuracy")
    plt.xlim(-0.02, 1.02)
    plt.ylim(-0.02, 1.02)
    plt.title("Per-document event alignment vs. temporal order")
    plt.legend(bbox_to_anchor=(1.02, 1), loc="upper left", fontsize=7, ncol=1)
    _save_current_figure(out, pdf)


def plot_corpus_counts(results: List[Dict[str, Any]], out: Path, pdf: bool) -> None:
    counts: Dict[str, int] = defaultdict(int)
    for r in results:
        if r.get("error") or r.get("skipped"):
            continue
        counts[str(r.get("corpus") or "unknown")] += 1
    if not counts:
        return
    labels = sorted(counts.keys(), key=lambda k: (-counts[k], k))
    vals = [counts[k] for k in labels]
    plt.figure(figsize=(7, 4))
    y = np.arange(len(labels))
    plt.barh(y, vals, color="steelblue")
    plt.yticks(y, labels)
    plt.xlabel("Number of scored documents")
    plt.title("Documents in this run (by corpus)")
    _save_current_figure(out, pdf)


def main() -> None:
    ap = argparse.ArgumentParser(description="Plot run_eval JSON summaries.")
    ap.add_argument(
        "--input",
        type=Path,
        default=ROOT / "outputs" / "latest_run.json",
        help="JSON written by src.run_eval",
    )
    ap.add_argument(
        "--out-dir",
        type=Path,
        default=ROOT / "outputs" / "figures",
        help="Directory for figure files",
    )
    ap.add_argument("--pdf", action="store_true", help="Also write vector PDFs")
    args = ap.parse_args()

    data = load_report(args.input)
    by_model = data.get("summary_by_model_and_tier") or {}
    if not by_model:
        print("No summary_by_model_and_tier in input; nothing to plot.", file=sys.stderr)
        sys.exit(1)

    args.out_dir.mkdir(parents=True, exist_ok=True)
    stem = args.input.stem
    tiers = ("easy", "hard")

    models_poa, mat_poa = summary_to_matrix(by_model, "mean_pairwise_order_accuracy", tiers)
    models_bm, mat_bm = summary_to_matrix(by_model, "mean_best_match_similarity", tiers)

    plot_grouped_bars_matrix(
        models_poa,
        mat_poa,
        tiers,
        "Mean pairwise order accuracy",
        "Aggregate metrics by tier and model",
        args.out_dir / f"{stem}_poa_by_tier.png",
        args.pdf,
    )
    plot_heatmap(
        models_poa,
        mat_poa,
        tiers,
        "Mean pairwise order accuracy",
        args.out_dir / f"{stem}_poa_heatmap.png",
        args.pdf,
    )
    plot_grouped_bars_matrix(
        models_bm,
        mat_bm,
        tiers,
        "Mean best-match similarity",
        "Fuzzy event alignment by tier and model",
        args.out_dir / f"{stem}_match_by_tier.png",
        args.pdf,
    )

    plot_scatter_per_doc(data.get("results", []), args.out_dir / f"{stem}_scatter_order_vs_match.png", args.pdf)
    plot_corpus_counts(data.get("results", []), args.out_dir / f"{stem}_corpus_counts.png", args.pdf)

    print(f"Wrote figures under {args.out_dir}")


if __name__ == "__main__":
    main()
