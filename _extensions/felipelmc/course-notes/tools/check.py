#!/usr/bin/env python3
"""Checagens do Course Notes.

Uso (na raiz do repositório):
    python3 _extensions/felipelmc/course-notes/tools/check.py pre
    python3 _extensions/felipelmc/course-notes/tools/check.py post [_book] [render.log]

pre   roda antes do `quarto render` (no CI, sem R):
      - toda aula, lista e trabalho está registrado no _quarto.yml;
      - toda página com blocos {r} tem _freeze/ em dia (o CI não executa R);
      - nenhuma página usa ::: {#refs} (quebra a bibliografia do livro);
      - nenhum caminho do Windows ou absoluto em .qmd/.R;
      - aulas têm `aula:` e o texto da barra lateral bate com o número.
post  roda depois do render:
      - nenhuma citação ou referência cruzada sem resolução;
      - links e imagens internos apontam para arquivos que existem.
"""
from __future__ import annotations

import glob
import hashlib
import html.parser
import json
import os
import re
import sys
import urllib.parse

try:
    import yaml
except ImportError:  # pragma: no cover
    sys.exit("check.py precisa do PyYAML: pip install pyyaml")

ERROS: list[str] = []
AVISOS: list[str] = []


def erro(msg: str) -> None:
    ERROS.append(msg)


def aviso(msg: str) -> None:
    AVISOS.append(msg)


def front_matter(path: str) -> dict:
    with open(path, encoding="utf-8") as f:
        txt = f.read()
    m = re.match(r"^---\s*\n(.*?)\n---\s*(\n|$)", txt, re.S)
    if not m:
        return {}
    try:
        return yaml.safe_load(m.group(1)) or {}
    except yaml.YAMLError as e:
        erro(f"{path}: cabeçalho YAML inválido ({e})")
        return {}


def ler_config() -> dict:
    with open("_quarto.yml", encoding="utf-8") as f:
        return yaml.safe_load(f) or {}


def capitulos(cfg: dict) -> list[tuple[str, str | None]]:
    """Lista (href, texto) de todos os capítulos e apêndices do livro."""
    out: list[tuple[str, str | None]] = []

    def walk(items):
        for it in items or []:
            if isinstance(it, str):
                out.append((it, None))
            elif isinstance(it, dict):
                if "part" in it:
                    if isinstance(it.get("part"), str) and it["part"].endswith(".qmd"):
                        out.append((it["part"], None))
                    walk(it.get("chapters"))
                elif "href" in it:
                    out.append((it["href"], it.get("text")))
                elif "file" in it:
                    out.append((it["file"], it.get("text")))

    book = cfg.get("book") or {}
    walk(book.get("chapters"))
    walk(book.get("appendices"))
    return out


def qmds() -> list[str]:
    ignorar = ("_book/", "_freeze/", "_extensions/", ".quarto/", "renv/", "node_modules/")
    out = []
    for p in glob.glob("**/*.qmd", recursive=True):
        p = p.replace(os.sep, "/")
        if p.startswith(ignorar) or "/_" in "/" + p:
            continue
        out.append(p)
    return sorted(out)


def subprojetos() -> list[str]:
    """Pastas com _quarto.yml próprio (ex.: slides) ficam fora do livro."""
    out = []
    for p in glob.glob("**/_quarto.yml", recursive=True):
        d = os.path.dirname(p).replace(os.sep, "/")
        if d and not d.startswith(("_book", "_extensions", "_freeze")):
            out.append(d + "/")
    return out


def check_pre() -> None:
    cfg = ler_config()
    caps = capitulos(cfg)
    hrefs = {h for h, _ in caps}
    subs = subprojetos()
    paginas = [p for p in qmds() if not any(p.startswith(s) for s in subs)]

    # 1. registro no _quarto.yml
    for p in paginas:
        precisa = (
            re.match(r"aulas/aula-[^/]+\.qmd$", p)
            or re.match(r"listas/[^/]+\.qmd$", p)
            or re.match(r"trabalhos/[^/]+/index\.qmd$", p)
        )
        if precisa and p not in hrefs:
            erro(f"{p}: não está em book.chapters no _quarto.yml")
    for h in hrefs:
        if h.endswith(".qmd") and not os.path.exists(h):
            erro(f"_quarto.yml: o capítulo {h} não existe")

    # 2. aulas: campo `aula` e texto da barra lateral
    textos = {h: t for h, t in caps}
    for p in paginas:
        if not re.match(r"aulas/aula-[^/]+\.qmd$", p):
            continue
        fm = front_matter(p)
        if "aula" not in fm:
            erro(f"{p}: falta o campo `aula:` no cabeçalho")
            continue
        if "title" not in fm:
            erro(f"{p}: falta o campo `title:` no cabeçalho")
        texto = textos.get(p)
        n = fm["aula"]
        if texto and isinstance(n, int):
            m = re.match(r"\s*(\d+)", str(texto))
            if m and int(m.group(1)) != n:
                erro(f"{p}: a barra lateral diz '{texto}', mas a página tem aula: {n}")

    # 3. conteúdo proibido
    win = re.compile(r"(?<![A-Za-z])[A-Za-z]:[\\/](?![\\/])")
    for p in paginas + sorted(glob.glob("**/*.R", recursive=True)):
        p = p.replace(os.sep, "/")
        if p.startswith(("_book/", "_freeze/", "_extensions/", "renv/")):
            continue
        with open(p, encoding="utf-8", errors="replace") as f:
            linhas = f.read().splitlines()
        for i, l in enumerate(linhas, 1):
            if p.endswith(".qmd") and re.search(r"^:::+\s*\{\s*#refs\b", l.strip()):
                erro(f"{p}:{i}: `::: {{#refs}}` não é permitido (as referências saem no fim de cada página)")
            if win.search(l) or "/Users/" in l or "OneDrive" in l or "C:\\" in l:
                erro(f"{p}:{i}: caminho absoluto ou do Windows: {l.strip()[:90]}")

    # 4. freeze em dia para páginas com R
    for p in paginas:
        with open(p, encoding="utf-8") as f:
            txt = f.read()
        if not re.search(r"^```+\s*\{r[\s,}]", txt, re.M):
            continue
        base = os.path.splitext(p)[0]
        fz = os.path.join("_freeze", base, "execute-results", "html.json")
        if not os.path.exists(fz):
            erro(f"{p}: sem _freeze. Rode `quarto render` localmente e faça o commit de _freeze/")
            continue
        with open(fz, encoding="utf-8") as f:
            h = json.load(f).get("hash")
        atual = hashlib.md5(txt.replace("\r\n", "\n").encode("utf-8")).hexdigest()
        if h != atual:
            erro(f"{p}: _freeze desatualizado. Rode `quarto render` localmente e faça o commit de _freeze/")

    # 5. avisos: chaves duplicadas na bibliografia
    bibs = cfg.get("bibliography") or []
    if isinstance(bibs, str):
        bibs = [bibs]
    chaves: dict[str, str] = {}
    for b in bibs:
        if not os.path.exists(b):
            erro(f"_quarto.yml: bibliografia {b} não existe")
            continue
        with open(b, encoding="utf-8") as f:
            for k in re.findall(r"^\s*@\w+\s*\{\s*([^,\s]+)\s*,", f.read(), re.M):
                if k in chaves:
                    aviso(f"{b}: chave duplicada `{k}`")
                chaves[k] = b


class Links(html.parser.HTMLParser):
    def __init__(self):
        super().__init__()
        self.refs: list[str] = []

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if tag == "a" and a.get("href"):
            self.refs.append(a["href"])
        if tag in ("img", "iframe", "script") and a.get("src"):
            self.refs.append(a["src"])


def check_post(out: str, log: str | None) -> None:
    if log and os.path.exists(log):
        with open(log, encoding="utf-8", errors="replace") as f:
            for l in f:
                if re.search(r"[Cc]itation .* not found|Unable to resolve crossref|WARN.*(crossref|citation)", l):
                    erro(f"render: {l.strip()}")
    for p in glob.glob(os.path.join(out, "**", "*.html"), recursive=True):
        rel = os.path.relpath(p, out)
        if rel.startswith(("site_libs", "_extensions")) or "/slides/" in "/" + rel.replace(os.sep, "/"):
            continue
        with open(p, encoding="utf-8", errors="replace") as f:
            txt = f.read()
        if "quarto-unresolved-ref" in txt:
            erro(f"{rel}: referência cruzada sem resolução")
        for k in re.findall(r"\?@([\w:.-]+)", re.sub(r"<(script|style)[^>]*>.*?</\1>", "", txt, flags=re.S)):
            erro(f"{rel}: citação sem resolução @{k}")
        parser = Links()
        parser.feed(txt)
        for ref in parser.refs:
            u = urllib.parse.urlparse(ref)
            if u.scheme or ref.startswith(("#", "//", "mailto:", "javascript:")) or not u.path:
                continue
            alvo = os.path.normpath(os.path.join(os.path.dirname(p), urllib.parse.unquote(u.path)))
            if u.path.startswith("/"):
                continue  # absoluto no domínio (ex.: /favicon.svg do site pessoal)
            if not os.path.exists(alvo):
                erro(f"{rel}: link quebrado para {ref}")


def main() -> int:
    if len(sys.argv) < 2 or sys.argv[1] not in ("pre", "post"):
        print(__doc__)
        return 2
    if sys.argv[1] == "pre":
        check_pre()
    else:
        out = sys.argv[2] if len(sys.argv) > 2 else "_book"
        log = sys.argv[3] if len(sys.argv) > 3 else None
        check_post(out, log)
    for a in AVISOS:
        print(f"aviso: {a}")
    for e in ERROS:
        print(f"erro:  {e}")
    if ERROS:
        print(f"\n{len(ERROS)} erro(s).")
        return 1
    print("ok: nenhuma pendência." if not AVISOS else f"ok, com {len(AVISOS)} aviso(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
