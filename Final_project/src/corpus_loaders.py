"""
Load tiered evaluation items from bundled JSON, NYT/COHA-style files,
TimeBank TimeML, and MATRES TSV (+ optional TempEval3 .tml for text).

Licensed corpora (NYT, full COHA, TimeBank LDC, TempEval3) are not redistributed;
place exports under data/corpora/ as described in data/corpora/README.txt.
"""

from __future__ import annotations

import csv
import json
import re
import xml.etree.ElementTree as ET
from collections import defaultdict, deque
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple

# --- helpers ---


def _local_tag(tag: str) -> str:
    if tag.startswith("{"):
        return tag.split("}", 1)[1]
    return tag


def _iter_by_local_name(root: ET.Element, name: str) -> Iterable[ET.Element]:
    n = name.lower()
    for el in root.iter():
        if _local_tag(el.tag).lower() == n:
            yield el


def _text_join(elem: ET.Element) -> str:
    return re.sub(r"\s+", " ", "".join(elem.itertext()).strip())


def _strip_html(s: str) -> str:
    # minimal HTML stripper for TimeBank-Dense unit exports
    s = re.sub(r"<[^>]+>", " ", s)
    return re.sub(r"\s+", " ", s).strip()


def _is_ei(s: Optional[str]) -> bool:
    return bool(s) and str(s).startswith("ei")


def _toposort(nodes: List[str], edges: List[Tuple[str, str]]) -> Optional[List[str]]:
    """Return topological order of nodes given directed edges u->v (u before v)."""
    node_set = set(nodes)
    edges = [(u, v) for u, v in edges if u in node_set and v in node_set]
    adj: Dict[str, Set[str]] = defaultdict(set)
    indeg: Dict[str, int] = defaultdict(int)
    for n in nodes:
        indeg.setdefault(n, 0)
    for u, v in edges:
        if v not in adj[u]:
            adj[u].add(v)
            indeg[v] += 1
    q = deque([n for n in nodes if indeg[n] == 0])
    order: List[str] = []
    while q:
        u = q.popleft()
        order.append(u)
        for v in adj[u]:
            indeg[v] -= 1
            if indeg[v] == 0:
                q.append(v)
    if len(order) != len(nodes):
        return None
    return order


def _union_find_merge(pairs: List[Tuple[str, str]], nodes: List[str]) -> Dict[str, str]:
    parent = {n: n for n in nodes}

    def find(x: str) -> str:
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a: str, b: str) -> None:
        ra, rb = find(a), find(b)
        if ra != rb:
            parent[rb] = ra

    for a, b in pairs:
        if a in parent and b in parent:
            union(a, b)
    return {n: find(n) for n in nodes}


def parse_timeml_file(path: Path) -> Dict[str, Any]:
    """
    Parse a TimeML / TimeBank .tml file: document text + gold event order from TLINKs.

    Uses EVENT / MAKEINSTANCE / TLINK elements (common in TimeBank/TempEval).
    """
    tree = ET.parse(path)
    root = tree.getroot()

    text_body = ""
    for text_el in _iter_by_local_name(root, "TEXT"):
        text_body = _text_join(text_el)
        break
    if not text_body:
        text_body = _text_join(root)

    eid_to_mention: Dict[str, str] = {}
    for ev in _iter_by_local_name(root, "EVENT"):
        eid = ev.get("eid") or ev.get("id")
        if not eid:
            continue
        eid_to_mention[eid] = _text_join(ev) or (ev.text or "").strip()

    eiid_to_eid: Dict[str, str] = {}
    for mi in _iter_by_local_name(root, "MAKEINSTANCE"):
        eiid = mi.get("eiid")
        eid = mi.get("eventID") or mi.get("eventid")
        if eiid and eid:
            eiid_to_eid[eiid] = eid

    eiids = list(eiid_to_eid.keys())
    edges: List[Tuple[str, str]] = []
    equal_pairs: List[Tuple[str, str]] = []

    for lk in _iter_by_local_name(root, "TLINK"):
        rel = (lk.get("relType") or lk.get("reltype") or "").upper()
        a = lk.get("eventInstanceID") or lk.get("eventinstanceid")
        b = lk.get("relatedToEventInstance") or lk.get("relatedtoeventinstance")
        if lk.get("relatedToTime") or lk.get("timeID"):
            continue
        if not _is_ei(a) or not _is_ei(b):
            continue
        if rel in ("BEFORE",):
            edges.append((a, b))
        elif rel in ("AFTER",):
            edges.append((b, a))
        elif rel in ("SIMULTANEOUS", "IDENTITY", "EQUAL"):
            equal_pairs.append((a, b))

    rep = _union_find_merge(equal_pairs, eiids)
    groups: Dict[str, List[str]] = defaultdict(list)
    for ei in eiids:
        groups[rep[ei]].append(ei)

    # Collapse equal eiids to one node per group (pick lexicographically first eiid as canon)
    canon: Dict[str, str] = {}
    for _, members in groups.items():
        head = sorted(members)[0]
        for m in members:
            canon[m] = head

    canon_nodes = sorted(set(canon.values()))
    canon_edges: List[Tuple[str, str]] = []
    for u, v in edges:
        cu, cv = canon.get(u, u), canon.get(v, v)
        if cu != cv:
            canon_edges.append((cu, cv))

    order = _toposort(canon_nodes, canon_edges) if canon_nodes else []
    if order is None:
        order = sorted(canon_nodes)

    gold_labels: List[str] = []
    for ei in order:
        eid = eiid_to_eid.get(ei)
        mention = eid_to_mention.get(eid, "") if eid else ""
        label = mention or ei
        gold_labels.append(label)

    doc_id = path.stem
    return {
        "id": f"timebank_{doc_id}",
        "tier": "easy",
        "corpus": "tempeval",
        "text": text_body,
        "gold_events_chronological": gold_labels,
        "gold_era_label": None,
        "gold_year_range": None,
        "notes": f"Parsed from TimeML file {path.name}",
    }


def load_timebank_dir(dir_path: Path, limit: Optional[int] = None) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    paths = sorted(dir_path.glob("*.tml"))
    n = len(paths) if limit is None else min(limit, len(paths))
    for p in paths[:n]:
        try:
            out.append(parse_timeml_file(p))
        except ET.ParseError:
            continue
    return out


def load_nyt_or_jsonl(path: Path) -> List[Dict[str, Any]]:
    """
    JSONL manifest: one JSON object per line with at least 'text' and 'id'.
    Optional: gold_events_chronological, tier, gold_year_range, corpus='nyt'.
    """
    rows: List[Dict[str, Any]] = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            obj = json.loads(line)
            tid = str(obj.get("id") or obj.get("doc_id") or obj.get("article_id") or "")
            text = obj.get("text") or obj.get("body") or obj.get("content")
            if not text:
                continue
            row = {
                "id": tid or f"nyt_{len(rows)}",
                "tier": obj.get("tier", "easy"),
                "corpus": obj.get("corpus", "nyt"),
                "text": text,
                "gold_events_chronological": obj.get("gold_events_chronological") or [],
                "gold_era_label": obj.get("gold_era_label"),
                "gold_year_range": obj.get("gold_year_range"),
                "notes": obj.get("notes"),
            }
            rows.append(row)
    return rows


def load_coha_txt_dir(dir_path: Path, limit: Optional[int] = None) -> List[Dict[str, Any]]:
    """One UTF-8 document per .txt file; id = filename stem. Gold lists optional (usually empty)."""
    out: List[Dict[str, Any]] = []
    paths = sorted(dir_path.glob("*.txt"))
    n = len(paths) if limit is None else min(limit, len(paths))
    for p in paths[:n]:
        text = p.read_text(encoding="utf-8", errors="replace")
        out.append(
            {
                "id": f"coha_{p.stem}",
                "tier": "easy",
                "corpus": "coha",
                "text": text,
                "gold_events_chronological": [],
                "gold_era_label": None,
                "gold_year_range": None,
                "notes": "COHA excerpt; add gold timelines in JSONL export if annotating.",
            }
        )
    return out


def _matres_build_gold_for_doc(rows: List[dict]) -> Tuple[List[str], Dict[str, str]]:
    """From MATRES rows for one doc, build ordered event labels and eiid->verb."""
    eiid_to_verb: Dict[str, str] = {}
    edges: List[Tuple[str, str]] = []
    equal_pairs: List[Tuple[str, str]] = []

    for r in rows:
        ei1 = str(r.get("eiid1") or r.get("eiid_1") or "").strip()
        ei2 = str(r.get("eiid2") or r.get("eiid_2") or "").strip()
        v1 = str(r.get("verb1") or "").strip()
        v2 = str(r.get("verb2") or "").strip()
        rel = str(r.get("relation") or "").strip().lower()
        if ei1 and v1 and ei1 not in eiid_to_verb:
            eiid_to_verb[ei1] = v1
        if ei2 and v2 and ei2 not in eiid_to_verb:
            eiid_to_verb[ei2] = v2
        if not ei1 or not ei2:
            continue
        if rel == "before":
            edges.append((ei1, ei2))
        elif rel == "after":
            edges.append((ei2, ei1))
        elif rel == "equal":
            equal_pairs.append((ei1, ei2))
        # vague: skip

    nodes = list(eiid_to_verb.keys())
    if not nodes:
        return [], {}
    rep = _union_find_merge(equal_pairs, nodes)
    canon = {n: rep[n] for n in nodes}
    canon_nodes = sorted(set(canon.values()))
    canon_edges: List[Tuple[str, str]] = []
    for u, v in edges:
        cu, cv = canon[u], canon[v]
        if cu != cv:
            canon_edges.append((cu, cv))

    order = _toposort(canon_nodes, canon_edges)
    if order is None:
        order = canon_nodes

    # Map canon eiid back to one verb
    labels: List[str] = []
    for ei in order:
        labels.append(eiid_to_verb.get(ei, ei))
    return labels, eiid_to_verb


def load_matres_tsv(
    tsv_path: Path,
    tml_dir: Optional[Path] = None,
    limit_docs: Optional[int] = None,
) -> List[Dict[str, Any]]:
    """
    MATRES format: header includes docid, verb1, verb2, eiid1, eiid2, relation (comma or tab).

    If tml_dir is set, load document text from ``{tml_dir}/{docid}.tml`` (TempEval3 layout).
    """
    with open(tsv_path, encoding="utf-8", newline="") as f:
        sample = f.read(4096)
        f.seek(0)
        try:
            dialect = csv.Sniffer().sniff(sample, delimiters=",\t;")
        except csv.Error:
            dialect = csv.excel
        reader = csv.DictReader(f, dialect=dialect)
        rows_raw = list(reader)

    by_doc: Dict[str, List[dict]] = defaultdict(list)
    for row in rows_raw:
        row = {(k or "").strip(): v for k, v in row.items()}
        docid = row.get("docid") or row.get("doc_id") or row.get("Docid")
        if not docid:
            continue
        by_doc[str(docid)].append(row)

    out: List[Dict[str, Any]] = []
    for i, (doc_id, rows) in enumerate(sorted(by_doc.items(), key=lambda x: x[0])):
        if limit_docs is not None and i >= limit_docs:
            break
        gold, _ = _matres_build_gold_for_doc(rows)
        text = ""
        if tml_dir:
            tml = tml_dir / f"{doc_id}.tml"
            if tml.is_file():
                try:
                    parsed = parse_timeml_file(tml)
                    text = parsed.get("text") or ""
                except (ET.ParseError, OSError):
                    text = ""
        item = {
            "id": str(doc_id),
            "tier": "easy",
            "corpus": "matres",
            "text": text,
            "gold_events_chronological": gold,
            "gold_era_label": None,
            "gold_year_range": None,
            "notes": "MATRES main-axis order from pairwise relations; text from TempEval3 .tml if provided.",
        }
        if not text.strip():
            item["skip_reason"] = (
                "No .tml text found; set --matres-tml-dir to TempEval3/te3-platinum etc."
            )
        out.append(item)
    return out


def load_tier_json(path: Path) -> List[Dict[str, Any]]:
    with open(path, encoding="utf-8") as f:
        data = json.load(f)
    for obj in data:
        obj.setdefault("corpus", "tier_samples")
    return data


def load_synthetic_json(path: Path, limit: Optional[int] = None) -> List[Dict[str, Any]]:
    """JSON array from synthetic_generator.py (same schema as tier_samples)."""
    with open(path, encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, list):
        raise ValueError("Synthetic file must be a JSON array.")
    if limit is not None:
        data = data[:limit]
    for obj in data:
        obj.setdefault("corpus", "synthetic")
        obj.setdefault("tier", "hard")
    return data


def load_te3_unit_csv(path: Path, limit_docs: Optional[int] = None) -> List[Dict[str, Any]]:
    """
    Load TempEval3 / TimeBank-Dense style unit CSVs (like TBDense_all_new.csv).

    These files are *not* MATRES relations; they contain per-event rows with a `bodytext`
    field that includes the document (often HTML). We deduplicate by `docid` and
    create documents with no gold timeline.
    """
    with open(path, encoding="utf-8", newline="") as f:
        sample = f.read(4096)
        f.seek(0)
        try:
            dialect = csv.Sniffer().sniff(sample, delimiters=",\t;")
        except csv.Error:
            dialect = csv.excel
        reader = csv.DictReader(f, dialect=dialect)
        rows = list(reader)

    docid_to_text: Dict[str, str] = {}
    for r in rows:
        docid = r.get("docid") or r.get("doc_id")
        body = r.get("bodytext") or r.get("body") or ""
        if not docid or not body:
            continue
        did = str(docid).strip()
        if did in docid_to_text:
            continue
        docid_to_text[did] = _strip_html(str(body))

    out: List[Dict[str, Any]] = []
    docids = sorted(docid_to_text.keys())
    if limit_docs is not None:
        docids = docids[:limit_docs]
    for did in docids:
        out.append(
            {
                "id": f"te3_{did}",
                "tier": "easy",
                "corpus": "tempeval",
                "text": docid_to_text[did],
                "gold_events_chronological": [],
                "gold_era_label": None,
                "gold_year_range": None,
                "notes": f"Loaded from TempEval3 unit CSV {path.name}; no gold order.",
            }
        )
    return out


def merge_samples(*lists: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    seen: Set[str] = set()
    out: List[Dict[str, Any]] = []
    for lst in lists:
        for s in lst:
            sid = str(s.get("id", ""))
            if sid in seen:
                sid = f"{sid}__dup{len(seen)}"
                s = {**s, "id": sid}
            seen.add(sid)
            out.append(s)
    return out
