"""Named model + backend for multi-model comparison runs."""

from __future__ import annotations

import os
from dataclasses import dataclass


@dataclass(frozen=True)
class ModelSpec:
    """Which provider and model id to call (open-weight / free APIs only)."""

    backend: str  # ollama | groq | huggingface
    model: str

    def key(self) -> str:
        return f"{self.backend}:{self.model}"

    @staticmethod
    def from_env() -> "ModelSpec":
        backend = os.environ.get("LLM_BACKEND", "ollama").lower().strip()
        if backend == "groq":
            model = os.environ.get("GROQ_MODEL", "llama-3.1-8b-instant")
        elif backend == "huggingface":
            model = os.environ.get("HF_MODEL", "meta-llama/Llama-3.2-3B-Instruct")
        else:
            backend = "ollama"
            model = os.environ.get("OLLAMA_MODEL", "llama3.2")
        return ModelSpec(backend=backend, model=model)

    @staticmethod
    def from_env_generator() -> "ModelSpec":
        """
        Model used only to *author* synthetic data. Should differ from models under test
        when possible (see data/synthetic/README.txt).
        """
        backend = os.environ.get("GENERATOR_BACKEND", "ollama").lower().strip()
        if backend == "groq":
            model = os.environ.get("GENERATOR_MODEL", "llama-3.1-8b-instant")
        elif backend == "huggingface":
            model = os.environ.get("GENERATOR_MODEL", os.environ.get("HF_MODEL", "meta-llama/Llama-3.2-3B-Instruct"))
        else:
            backend = "ollama"
            model = os.environ.get("GENERATOR_MODEL", "mistral")
        return ModelSpec(backend=backend, model=model)


def parse_model_args(specs: list[str]) -> list[ModelSpec]:
    """
    Parse repeated CLI entries like 'ollama:llama3.2' or 'groq:llama-3.1-8b-instant'.
    Only the first ':' splits backend from model id (HF ids may contain ':' rarely).
    """
    out: list[ModelSpec] = []
    for raw in specs:
        s = raw.strip()
        if not s:
            continue
        if ":" not in s:
            raise ValueError(
                f"Invalid --model {raw!r}; expected backend:model_id (e.g. ollama:llama3.2)."
            )
        backend, model = s.split(":", 1)
        backend = backend.strip().lower()
        model = model.strip()
        if backend not in ("ollama", "groq", "huggingface"):
            raise ValueError(f"Unknown backend {backend!r} in {raw!r}.")
        if not model:
            raise ValueError(f"Missing model id in {raw!r}.")
        out.append(ModelSpec(backend=backend, model=model))
    return out
