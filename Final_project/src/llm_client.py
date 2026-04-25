"""Open-weight models via Ollama (local), Groq, or Hugging Face Inference API."""

from __future__ import annotations

import json
import os
from typing import Any, Dict, List, Optional

import httpx

from src.model_spec import ModelSpec


class LLMError(RuntimeError):
    pass


def _strip_json_fence(text: str) -> str:
    t = text.strip()
    if t.startswith("```"):
        lines = t.split("\n")
        if lines[0].startswith("```"):
            lines = lines[1:]
        if lines and lines[-1].strip() == "```":
            lines = lines[:-1]
        t = "\n".join(lines)
    return t.strip()


def parse_json_response(text: str) -> Dict[str, Any]:
    raw = _strip_json_fence(text)
    try:
        return json.loads(raw)
    except json.JSONDecodeError as e:
        raise LLMError(f"Model did not return valid JSON: {e}\n---\n{raw[:2000]}") from e


def _json_retry_enabled() -> bool:
    raw = os.environ.get("LLM_JSON_RETRY", "1").strip().lower()
    return raw not in ("0", "false", "no", "off")


def _repair_json_prompt(*, bad_text: str, system_schema: str) -> List[Dict[str, str]]:
    """
    Ask the same model to rewrite its output as valid JSON.
    """
    sys = (
        "You are a strict JSON repair tool.\n"
        "Return ONLY valid JSON (no markdown, no commentary).\n"
        "Do not add new facts; preserve meaning.\n\n"
        f"Target schema:\n{system_schema}\n"
    )
    usr = (
        "Rewrite the following into valid JSON that matches the schema.\n\n"
        "--- BEGIN BAD OUTPUT ---\n"
        f"{bad_text}\n"
        "--- END BAD OUTPUT ---\n"
    )
    return [{"role": "system", "content": sys}, {"role": "user", "content": usr}]


def chat_ollama(
    messages: List[Dict[str, str]],
    *,
    base_url: str,
    model: str,
    timeout: float = 120.0,
) -> str:
    url = base_url.rstrip("/") + "/api/chat"
    payload = {"model": model, "messages": messages, "stream": False}
    r = httpx.post(url, json=payload, timeout=timeout)
    r.raise_for_status()
    data = r.json()
    msg = data.get("message", {})
    content = msg.get("content")
    if not content:
        raise LLMError(f"Unexpected Ollama response: {data}")
    return content


def chat_groq(
    messages: List[Dict[str, str]],
    *,
    api_key: str,
    model: str,
    timeout: float = 120.0,
) -> str:
    url = "https://api.groq.com/openai/v1/chat/completions"
    headers = {"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"}
    payload = {
        "model": model,
        "messages": messages,
        "temperature": 0.2,
    }
    r = httpx.post(url, json=payload, headers=headers, timeout=timeout)
    r.raise_for_status()
    data = r.json()
    try:
        return data["choices"][0]["message"]["content"]
    except (KeyError, IndexError) as e:
        raise LLMError(f"Unexpected Groq response: {data}") from e


def chat_huggingface(
    messages: List[Dict[str, str]],
    *,
    token: str,
    model: str,
    timeout: float = 120.0,
) -> str:
    """Chat-style HF inference for instruction-tuned models that accept conversational input."""
    url = f"https://api-inference.huggingface.co/models/{model}"
    headers = {"Authorization": f"Bearer {token}"}
    # Many HF chat models expect a single 'inputs' string; we flatten messages.
    prompt_parts = []
    for m in messages:
        role = m["role"].upper()
        prompt_parts.append(f"{role}: {m['content']}")
    prompt_parts.append("ASSISTANT:")
    prompt = "\n\n".join(prompt_parts)
    payload: Dict[str, Any] = {"inputs": prompt, "parameters": {"max_new_tokens": 1024, "temperature": 0.2}}
    r = httpx.post(url, json=payload, headers=headers, timeout=timeout)
    if r.status_code == 503:
        raise LLMError("Hugging Face model is loading; retry in ~30s.")
    r.raise_for_status()
    data = r.json()
    if isinstance(data, list) and data and "generated_text" in data[0]:
        gen = data[0]["generated_text"]
        if isinstance(gen, str) and "ASSISTANT:" in gen:
            return gen.split("ASSISTANT:")[-1].strip()
        return str(gen).strip()
    if isinstance(data, dict) and "generated_text" in data:
        return str(data["generated_text"]).strip()
    raise LLMError(f"Unexpected HF response: {data!r}")


def _ollama_base() -> str:
    return os.environ.get("OLLAMA_BASE_URL", "http://127.0.0.1:11434")


def _ollama_timeout() -> float:
    """Local models (esp. first load + large JSON) often exceed 120s on laptop CPUs."""
    raw = os.environ.get("OLLAMA_TIMEOUT", "600")
    try:
        return max(30.0, float(raw))
    except ValueError:
        return 600.0


def complete_json_task(
    system: str,
    user: str,
    model_spec: Optional[ModelSpec] = None,
) -> Dict[str, Any]:
    messages = [
        {"role": "system", "content": system},
        {"role": "user", "content": user},
    ]
    spec = model_spec or ModelSpec.from_env()

    def _chat(msgs: List[Dict[str, str]]) -> str:
        if spec.backend == "groq":
            key = os.environ.get("GROQ_API_KEY")
            if not key:
                raise LLMError("GROQ_API_KEY is not set (or use ollama backend).")
            return chat_groq(msgs, api_key=key, model=spec.model)
        if spec.backend == "huggingface":
            tok = os.environ.get("HF_TOKEN")
            if not tok:
                raise LLMError("HF_TOKEN is not set (or use ollama backend).")
            return chat_huggingface(msgs, token=tok, model=spec.model)
        try:
            return chat_ollama(
                msgs,
                base_url=_ollama_base(),
                model=spec.model,
                timeout=_ollama_timeout(),
            )
        except httpx.ReadTimeout as e:
            raise LLMError(
                "Ollama request timed out. Large models or cold start can be slow; "
                "set OLLAMA_TIMEOUT=900 in .env, or run `ollama run <model>` once to load it."
            ) from e

    text = _chat(messages)
    try:
        return parse_json_response(text)
    except LLMError as first_err:
        if not _json_retry_enabled():
            raise
        repaired_text = _chat(_repair_json_prompt(bad_text=text, system_schema=system))
        try:
            return parse_json_response(repaired_text)
        except LLMError:
            raise first_err
