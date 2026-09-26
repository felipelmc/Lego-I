#!/usr/bin/env python3
"""Acrescenta DOIs a entradas @article/@incollection de um .bib, consultando o Crossref.

Uso: python3 _extensions/felipelmc/course-notes/tools/add_dois.py references.bib [--dry-run]

Só grava o DOI quando o título devolvido pelo Crossref é quase idêntico ao da entrada
(similaridade >= 0,92) e o ano bate (±1). Entradas que já têm doi ficam como estão.
"""
import difflib
import json
import re
import ssl
import sys
import time
import urllib.error
import urllib.parse
import urllib.request


def limpa(t: str) -> str:
    t = re.sub(r"[{}\\`'\"]", "", t)
    return " ".join(t.lower().split())


def _contexto_ssl():
    try:
        import certifi  # o Python do python.org no macOS não usa os certificados do sistema
        return ssl.create_default_context(cafile=certifi.where())
    except ImportError:
        return ssl.create_default_context()


SSL = _contexto_ssl()


def consulta(titulo: str, autor: str) -> list[dict]:
    q = urllib.parse.urlencode({"query.bibliographic": f"{titulo} {autor}", "rows": 6,
                                "select": "DOI,title,issued,type,container-title"})
    req = urllib.request.Request(f"https://api.crossref.org/works?{q}",
                                 headers={"User-Agent": "course-notes-add-dois/0.1"})
    for tentativa in range(3):
        try:
            with urllib.request.urlopen(req, timeout=30, context=SSL) as r:
                return json.load(r)["message"]["items"]
        except urllib.error.HTTPError as e:
            if e.code != 429 or tentativa == 2:
                raise
            time.sleep(5 * (tentativa + 1))
    return []


def main() -> int:
    if len(sys.argv) < 2:
        print(__doc__)
        return 2
    path, dry = sys.argv[1], "--dry-run" in sys.argv
    s = open(path, encoding="utf-8").read()
    novas = 0

    def trata(m):
        nonlocal novas
        tipo, chave, corpo = m.group(1), m.group(2), m.group(3)
        if tipo.lower() not in ("article", "incollection", "inproceedings") or re.search(r"\bdoi\s*=", corpo, re.I):
            return m.group(0)
        tit = re.search(r"\btitle\s*=\s*[{\"](.+?)[}\"]\s*,?\s*\n", corpo, re.S)
        ano = re.search(r"\byear\s*=\s*[{\"]?(\d{4})", corpo)
        aut = re.search(r"\bauthor\s*=\s*[{\"](.+?)[}\"]\s*,?\s*\n", corpo, re.S)
        if not tit:
            return m.group(0)
        titulo = limpa(tit.group(1))
        rev = re.search(r"\b(journal|booktitle)\s*=\s*[{\"](.+?)[}\"]\s*,?\s*\n", corpo, re.S)
        revista = limpa(rev.group(2)) if rev else ""
        tipos_ok = {"article": {"journal-article"}, "incollection": {"book-chapter", "reference-entry"},
                    "inproceedings": {"proceedings-article"}}[tipo.lower()]
        sobrenome = (aut.group(1).split(",")[0] if aut else "")
        try:
            itens = consulta(titulo, sobrenome)
        except Exception as e:  # rede
            print(f"{chave}: erro na consulta ({e})")
            return m.group(0)
        time.sleep(1.0)
        for it in itens:
            t = limpa((it.get("title") or [""])[0])
            y = (it.get("issued", {}).get("date-parts") or [[None]])[0][0]
            sim = difflib.SequenceMatcher(None, titulo, t).ratio()
            cont = limpa((it.get("container-title") or [""])[0])
            if it.get("type") not in tipos_ok:
                continue
            if revista and cont and difflib.SequenceMatcher(None, revista, cont).ratio() < 0.6:
                continue
            if sim >= 0.92 and (not ano or not y or abs(int(ano.group(1)) - y) <= 1):
                doi = it["DOI"]
                print(f"{chave}: {doi}  (similaridade {sim:.2f})")
                novas += 1
                corpo2 = corpo.rstrip() + f",\n  doi = {{{doi}}}"
                corpo2 = corpo2.replace(",,", ",")
                return f"@{tipo}{{{chave},{corpo2}\n}}"
        print(f"{chave}: sem DOI confiável")
        return m.group(0)

    s2 = re.sub(r"@(\w+)\s*\{\s*([^,\s]+)\s*,(.*?)\n\}", trata, s, flags=re.S)
    if not dry and novas:
        open(path, "w", encoding="utf-8").write(s2)
    print(f"{novas} DOI(s) {'encontrado(s)' if dry else 'gravado(s)'}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
