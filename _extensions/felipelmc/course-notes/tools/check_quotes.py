#!/usr/bin/env python3
"""Confere que as citações literais (linhas com `>`) não mudaram entre duas versões.

Uso (na raiz do repositório):
    python3 _extensions/felipelmc/course-notes/tools/check_quotes.py <base> [<head>]

<base> e <head> são referências do git (padrão de <head>: a árvore de trabalho).
Normaliza espaços, remove a referência final `(p. N)` ou `[@chave, p. N]` e compara
bloco a bloco. Mudanças permitidas vão em tools/quote-allowlist.yml do repositório:
    - file: aulas/aula-10.qmd
      before: "texto normalizado antes"
      after: "texto normalizado depois"
Uma citação que virou parte de uma caixa (ex.: nota-ia) com o mesmo texto também passa.
"""
from __future__ import annotations

import difflib
import os
import re
import subprocess
import sys
import unicodedata

CITE = re.compile(r"\s*(\((?:p|pp)\.?\s*[\d\-–, ]+\)|\[@[^\]]+\])\s*\.?\s*$")


def git(*args: str) -> str:
    return subprocess.run(["git", *args], capture_output=True, text=True).stdout


def ler(ref: str | None, path: str) -> str | None:
    if ref is None:
        return open(path, encoding="utf-8").read() if os.path.exists(path) else None
    out = subprocess.run(["git", "show", f"{ref}:{path}"], capture_output=True, text=True)
    return out.stdout if out.returncode == 0 else None


def norm(s: str) -> str:
    s = unicodedata.normalize("NFC", s)
    s = " ".join(s.split())
    return s


def quotes(text: str) -> list[str]:
    blocos, atual, cerca = [], [], False
    for l in text.splitlines():
        if l.lstrip().startswith("```"):
            cerca = not cerca
        if not cerca and re.match(r"^\s{0,3}>", l):
            atual.append(re.sub(r"^\s{0,3}>\s?", "", l))
            continue
        if atual:
            blocos.append(atual)
            atual = []
    if atual:
        blocos.append(atual)
    out = []
    for b in blocos:
        t = norm(" ".join(b))
        t = CITE.sub("", t)
        if t.strip():
            out.append(t)
    return out


def main() -> int:
    if len(sys.argv) < 2:
        print(__doc__)
        return 2
    base = sys.argv[1]
    head = sys.argv[2] if len(sys.argv) > 2 else None
    allow = {}
    if os.path.exists("tools/quote-allowlist.yml"):
        import yaml
        for a in yaml.safe_load(open("tools/quote-allowlist.yml", encoding="utf-8")) or []:
            allow[(a["file"], norm(a["before"]))] = norm(a["after"])
    diff = git("diff", "--name-status", "-M", base, *( [head] if head else [] ))
    ren = {}
    for l in diff.splitlines():
        p = l.split("\t")
        if p[0].startswith("R") and len(p) == 3:
            ren[p[1]] = p[2]
    ruins = 0
    for f in git("ls-tree", "-r", "--name-only", base).split():
        if not f.endswith(".qmd"):
            continue
        antigo = ler(base, f)
        novo_path = ren.get(f, f)
        novo = ler(head, novo_path)
        if novo is None:
            qs = quotes(antigo or "")
            if qs:
                print(f"[ARQUIVO REMOVIDO] {f}: {len(qs)} citação(ões)")
                ruins += len(qs)
            continue
        depois = quotes(novo)
        plano = norm(novo)
        for q in quotes(antigo or ""):
            alvo = allow.get((novo_path, q), q)
            if alvo in depois:
                depois.remove(alvo)
                continue
            if alvo in plano:
                continue
            ruins += 1
            m = difflib.get_close_matches(alvo, depois, 1, 0.5)
            print(f"[ALTERADA] {novo_path}\n  - {alvo[:240]}\n  + {(m[0] if m else '(não encontrada)')[:240]}")
        for q in depois:
            print(f"[NOVA] {novo_path}: {q[:160]}")
    if ruins:
        print(f"\n{ruins} citação(ões) alterada(s) ou removida(s).")
        return 1
    print("ok: citações idênticas.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
