"""Mask explicit temporal expressions to reduce date/year leakage."""

from __future__ import annotations

import re


# Years (common forms)
_YEAR = re.compile(
    r"\b(?:(?:[12][0-9]{3})|(?:'?\d{2}))\b"
)

# Month name + day + optional year
_MONTH_DAY = re.compile(
    r"\b(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|"
    r"Aug(?:ust)?|Sep(?:t(?:ember)?)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\s+"
    r"\d{1,2}(?:st|nd|rd|th)?(?:\s*,\s*(?:[12][0-9]{3}|'?\d{2}))?",
    re.IGNORECASE,
)

# Numeric dates like 06/06/1944 or 1944-06-06
_NUMERIC_DATE = re.compile(
    r"\b\d{1,2}[/-]\d{1,2}[/-]\d{2,4}\b|\b\d{4}[/-]\d{1,2}[/-]\d{1,2}\b"
)

# Decades / centuries
_DECADE = re.compile(r"\b(?:18|19|20)\d0s\b|\b\d{1,2}(?:st|nd|rd|th)\s+century\b", re.IGNORECASE)

# "on Monday", "last Tuesday" — light touch (can remove discourse cues; optional)
_DAY_REF = re.compile(
    r"\b(?:last|next|this)\s+(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)\b",
    re.IGNORECASE,
)

# BC / AD
_ERA = re.compile(r"\b(?:B\.?C\.?E?|A\.?D\.?)\b", re.IGNORECASE)


def mask_temporal(text: str, aggressive: bool = False) -> str:
    """
    Replace explicit temporal cues with placeholders.

    aggressive: if True, also mask relative day references like 'last Tuesday'.
    """
    out = text
    out = _NUMERIC_DATE.sub("[DATE]", out)
    out = _MONTH_DAY.sub("[DATE]", out)
    out = _YEAR.sub("[YEAR]", out)
    out = _DECADE.sub("[ERA]", out)
    out = _ERA.sub("[ERA]", out)
    if aggressive:
        out = _DAY_REF.sub("[DAY]", out)
    # Collapse duplicate spaces
    out = re.sub(r"\s{2,}", " ", out).strip()
    return out
