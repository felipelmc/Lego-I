#!/usr/bin/env bash
# Gera o PDF de um trabalho a partir do mesmo index.qmd usado no livro.
# O trabalho é renderizado numa cópia temporária, fora do projeto, para que as opções
# `format: pdf` do próprio arquivo valham (dentro do livro, só o HTML é gerado).
#
# Uso (na raiz do repositório):
#   _extensions/felipelmc/course-notes/tools/pdf.sh trabalhos/tarefa-1 [arquivo.qmd] [saida.pdf]
set -euo pipefail

dir="${1:?informe a pasta do trabalho, ex.: trabalhos/tarefa-1}"
qmd="${2:-index.qmd}"
slug="$(basename "$dir")"
out="${3:-$slug.pdf}"
root="$(pwd)"

[ -f "$dir/$qmd" ] || { echo "Não encontrei $dir/$qmd" >&2; exit 1; }

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cp -R "$dir"/. "$tmp"/
cp -R "$root/_extensions" "$tmp/_extensions"
touch "$tmp/.here"

# Sem bibliografia própria, o trabalho usa a do projeto (inserida no cabeçalho da cópia).
if ! grep -qE '^bibliography:' "$dir/$qmd" && [ -f "$root/references.bib" ]; then
  cp "$root/references.bib" "$tmp/_referencias-projeto.bib"
  awk 'NR==1 && /^---/ {print; print "bibliography: _referencias-projeto.bib"; next} {print}' \
    "$dir/$qmd" > "$tmp/$qmd"
fi

( cd "$tmp" && quarto render "$qmd" --to pdf )

pdf="${qmd%.qmd}.pdf"
cp "$tmp/$pdf" "$dir/$out"
echo "PDF salvo em $dir/$out"
