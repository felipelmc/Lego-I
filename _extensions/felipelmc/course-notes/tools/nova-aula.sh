#!/usr/bin/env bash
# Cria aulas/aula-NN.qmd a partir do modelo e imprime a linha para o _quarto.yml.
# Uso: _extensions/felipelmc/course-notes/tools/nova-aula.sh 4 "Título da aula" [2026-04-01]
set -euo pipefail
n="${1:?número da aula}"; titulo="${2:?título da aula}"; data="${3:-}"
nn="$(printf '%02d' "$n")"
arq="aulas/aula-$nn.qmd"
[ -e "$arq" ] && { echo "$arq já existe" >&2; exit 1; }
mkdir -p aulas
{
  echo "---"
  echo "title: \"$titulo\""
  echo "aula: $n"
  [ -n "$data" ] && echo "date: $data"
  echo "description: \"\""
  echo "---"
  echo
  echo "## Leituras"
  echo
  echo "### @chave2026, cap. 1"
  echo
  echo "> Citação literal. [@chave2026, p. 1]"
  echo
  echo "## Anotações de aula"
  echo
} > "$arq"
echo "Criei $arq. Acrescente ao _quarto.yml, na parte \"Aulas\":"
echo "        - href: $arq"
echo "          text: \"$nn · $titulo\""
