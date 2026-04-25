"""Scoring predicted timelines and era estimates against gold."""

from __future__ import annotations

import re
from difflib import SequenceMatcher
from typing import Iterable, List, Optional, Sequence, Tuple


def _norm(s: str) -> str:
    return re.sub(r"\s+", " ", s.strip().lower())


def _similarity(a: str, b: str) -> float:
    return SequenceMatcher(None, _norm(a), _norm(b)).ratio()


def align_gold_to_predicted(
    gold: Sequence[str], predicted: Sequence[str], threshold: float = 0.45
) -> List[Optional[int]]:
    """For each gold event, pick best matching predicted line index (greedy, unique)."""
    used: set[int] = set()
    mapping: List[Optional[int]] = []
    for g in gold:
        best_i: Optional[int] = None
        best_s = threshold
        for i, p in enumerate(predicted):
            if i in used:
                continue
            s = _similarity(g, p)
            if s > best_s:
                best_s = s
                best_i = i
        if best_i is not None:
            used.add(best_i)
        mapping.append(best_i)
    return mapping


def pairwise_order_accuracy(
    gold: Sequence[str], predicted: Sequence[str], threshold: float = 0.45
) -> Tuple[float, int, int]:
    """
    Among pairs (i,j) with i<j in gold, count fraction where matched predicted
    indices preserve order (pred_i < pred_j).
    """
    m = align_gold_to_predicted(list(gold), list(predicted), threshold=threshold)
    idxs = [x for x in m if x is not None]
    if len(idxs) < 2:
        return 0.0, 0, 0
    good = 0
    total = 0
    for a in range(len(m)):
        for b in range(a + 1, len(m)):
            ia, ib = m[a], m[b]
            if ia is None or ib is None:
                continue
            total += 1
            if ia < ib:
                good += 1
    if total == 0:
        return 0.0, 0, 0
    return good / total, good, total


def mean_reciprocal_rank_match(gold: Sequence[str], predicted: Sequence[str]) -> float:
    """Average similarity of best match per gold line (recall-oriented)."""
    scores: List[float] = []
    for g in gold:
        if not predicted:
            scores.append(0.0)
            continue
        best = max(_similarity(g, p) for p in predicted)
        scores.append(best)
    return sum(scores) / max(len(scores), 1)


def year_range_overlap(
    pred_range: Optional[Tuple[Optional[int], Optional[int]]],
    gold_range: Optional[Tuple[int, int]],
) -> Optional[float]:
    """
    Jaccard-like overlap on integer years [a,b] inclusive; None gold skips.
    """
    if gold_range is None or pred_range is None:
        return None
    pa, pb = pred_range
    if pa is None or pb is None:
        return None
    ga, gb = gold_range
    lo = max(min(pa, pb), ga)
    hi = min(max(pa, pb), gb)
    if hi < lo:
        return 0.0
    inter = hi - lo + 1
    span_pred = abs(pb - pa) + 1
    span_gold = gb - ga + 1
    union = span_pred + span_gold - inter
    return inter / union if union else 0.0


def parse_year_range_from_json(val) -> Optional[Tuple[Optional[int], Optional[int]]]:
    if val is None:
        return None
    if isinstance(val, (list, tuple)) and len(val) == 2:
        try:
            a = int(val[0]) if val[0] is not None else None
            b = int(val[1]) if val[1] is not None else None
            return (a, b)
        except (TypeError, ValueError):
            return None
    return None


def summarize_by_model_and_tier(rows: Iterable[dict]) -> dict:
    """Group metrics by model key (backend:model_id) and tier."""
    by_model: dict = {}
    for r in rows:
        if r.get("error"):
            continue
        mk = r.get("model") or "unknown"
        t = r.get("tier", "unknown")
        by_model.setdefault(mk, {}).setdefault(t, []).append(r)
    out: dict = {}
    for mk, tiers in by_model.items():
        out[mk] = {}
        for tier, lst in tiers.items():
            poa = [x["pairwise_order_accuracy"] for x in lst if x.get("pairwise_order_accuracy") is not None]
            mrr = [x["mean_best_match"] for x in lst if x.get("mean_best_match") is not None]
            yrs = [x["year_overlap"] for x in lst if x.get("year_overlap") is not None]
            out[mk][tier] = {
                "n": len(lst),
                "mean_pairwise_order_accuracy": sum(poa) / len(poa) if poa else None,
                "mean_best_match_similarity": sum(mrr) / len(mrr) if mrr else None,
                "mean_year_overlap": sum(yrs) / len(yrs) if yrs else None,
            }
    return out


def summarize_tier_results(rows: Iterable[dict]) -> dict:
    by_tier: dict = {}
    for r in rows:
        t = r.get("tier", "unknown")
        by_tier.setdefault(t, []).append(r)
    out = {}
    for tier, lst in by_tier.items():
        poa = [x["pairwise_order_accuracy"] for x in lst if x.get("pairwise_order_accuracy") is not None]
        mrr = [x["mean_best_match"] for x in lst if x.get("mean_best_match") is not None]
        yrs = [x["year_overlap"] for x in lst if x.get("year_overlap") is not None]
        out[tier] = {
            "n": len(lst),
            "mean_pairwise_order_accuracy": sum(poa) / len(poa) if poa else None,
            "mean_best_match_similarity": sum(mrr) / len(mrr) if mrr else None,
            "mean_year_overlap": sum(yrs) / len(yrs) if yrs else None,
        }
    return out
