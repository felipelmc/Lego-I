# CLAUDE.md

Orientações para o Claude Code neste repositório de anotações de disciplina.

## O que é

Um livro Quarto com as anotações de uma disciplina, publicado em `felipelamarca.com/<repo>/`. O visual e os componentes vêm da extensão `_extensions/felipelmc/course-notes/`, mantida em [felipelmc/Course-Notes-Template](https://github.com/felipelmc/Course-Notes-Template). O `_quarto.yml` do repositório só tem os dados da disciplina e a ordem dos capítulos.

## Comandos

```bash
quarto preview                                              # servidor local com reload
quarto render                                               # renderiza tudo e atualiza _freeze/
python3 _extensions/felipelmc/course-notes/tools/check.py pre        # checagens do CI
python3 _extensions/felipelmc/course-notes/tools/check.py post _book
_extensions/felipelmc/course-notes/tools/nova-aula.sh 4 "Título" 2026-04-01
_extensions/felipelmc/course-notes/tools/pdf.sh trabalhos/tarefa-1  # PDF de um trabalho
quarto update extension felipelmc/Course-Notes-Template     # atualizar o template
```

## Publicação e `_freeze/`

- O CI (`.github/workflows/publish.yml`) **não executa R**. Ele renderiza com os resultados em `_freeze/`, que vai para o git, e publica no GitHub Pages (modo "GitHub Actions").
- Depois de editar qualquer página com blocos `{r}`, rode `quarto render` localmente e faça o commit de `_freeze/` junto. Se não fizer, `check.py pre` falha no CI.
- Pacotes de R usados nas páginas ficam listados no `DESCRIPTION`.
- Nunca faça o commit de `_book/` e nunca use `output-dir: docs`.

## Estrutura e convenções

- Aulas em `aulas/aula-NN.qmd` (dois dígitos). Cabeçalho: `title`, `aula` (número), `date` (opcional), `description`. Toda aula nova precisa entrar em `book.chapters` no `_quarto.yml`, com `text: "NN · Título"`.
- Esqueleto da aula com leituras: `## Leituras` (com `### @chave, cap. N` por texto lido), `## Anotações de aula`, `## Simulação` (opcional). Aula sem leituras: os tópicos ficam direto em `##`, e a simulação vem no fim.
- Citações literais: `> texto [@chave, p. N]`. Uma única bibliografia, `references.bib`, com chaves `sobrenomeANOpalavra`.
- **Nunca use `::: {#refs}`**: num livro, ele junta a bibliografia inteira numa página e esconde as referências das outras. As referências saem sozinhas no fim de cada página.
- Nunca declare `format: html` no `_quarto.yml` nem nas páginas; o formato vem da extensão.
- Trabalhos em `trabalhos/<slug>/index.qmd`, com os campos `tipo`, `date`, `description`, `image`, `metodos`, `dados`, `materiais` e `abstract` (ver `guia/trabalhos.qmd` no template). O PDF entregue fica na pasta e é a versão oficial. A vitrine **não mostra notas**.
- Slides ficam em `trabalhos/<slug>/slides/`, com `_quarto.yml` próprio (`type: default`) e `embed-resources: true`. Renderize com `quarto render trabalhos/<slug>/slides` e faça o commit do `index.html`.
- Simulações em OJS ficam no próprio arquivo da aula, numa seção `## Simulação` no fim (nunca em arquivo incluído: o `_freeze` só enxerga o texto do arquivo da aula); o texto que as apresenta, se escrito com IA, vai em `::: {.nota-ia collapse="false" ...}`. Importe de `/_extensions/felipelmc/course-notes/ojs/notes.js`, use `rng(semente)` e `palette(scheme())`, envolva em `::: cn-sim` dentro de `::: panel-tabset` com as abas "Interativo" e "Em R" (esta com `#| eval: false`).
- Figuras em R: `source(here::here("_extensions/felipelmc/course-notes/r/notes.R"))` e `theme_notes()`. Caminhos de dados sempre com `here::here()`, nunca absolutos.

## Matemática

- Linha em branco antes e depois de `$$`; `aligned` para alinhar.
- `^\top` para transposta, `\mathbb{E}`, `\mathbb{V}`, `\text{Cov}`, `\perp\!\!\!\perp`.
- Ambientes: `::: {#def-...}`, `::: {#thm-...}`, `::: prova` (recolhível).
- Resultado central: `$\boxed{...}$`.
- Opções de bloco em `#| chave: valor`, nunca `{r, eval=F}`.

## Política de conteúdo

- **Citações são literais.** Não corrija nada dentro de `>`, a não ser trocar `(p. N)` por `[@chave, p. N]` ou consertar LaTeX quebrado. Citação em outra língua fica na língua original.
- **Os comentários e as anotações de aula são do autor.** Polimento leve é permitido (ortografia, concordância, palavra faltando, travessão, títulos em *sentence case*). Não mude afirmações, números, ordem dos argumentos nem a língua, e não apague coloquialismos.
- **Texto escrito com IA vai sempre em `::: nota-ia`** (use `ferramenta="NotebookLM"` etc. quando não for o Claude). Nunca misture explicação gerada com a voz do autor fora dessa caixa.
- Erros de conteúdo (uma derivação errada, uma afirmação duvidosa) são apontados ao autor, e não corrigidos por conta própria.
- Nunca invente citação, página ou referência.

## Estilo de escrita do autor

- Sem negrito no corpo do texto (só na primeira ocorrência de um termo definido) e sem travessão; use vírgula, dois-pontos ou parênteses.
- Dois-pontos e ponto e vírgula em enumerações são bem-vindos.
- Conectivos dele: "De fato", "Note que", "Em particular", "por sua vez", "isto é", "sobretudo", "Por fim". Nunca: "Com efeito", "Nesse sentido", "Dito isso", "Cabe destacar", "É importante notar".
- Evite marcas de IA: crucial, inovador, abrangente, robusto (fora do sentido técnico), sinergia, panorama, cenário (metafórico), ademais, notavelmente, potencializar.
- Números com vírgula decimal e `%`. Títulos em *sentence case*. Termos técnicos em inglês em itálico.
- Comentários em scripts de R e Python são escritos sem acentuação, de propósito.

## Esta disciplina

Lego I: introdução à pesquisa social quantitativa (IESP-UERJ, 2025.1), com Bruno Marques Schaefer. As aulas estão agrupadas em duas partes do `_quarto.yml`, como na ementa (`ementa.pdf`): desenho de pesquisa (aulas 01 a 05) e estatística (06 a 13).

- As aulas 03 e 04 foram sessões práticas de `R` e não têm anotações; as aulas 14 e 15 também não. Todas aparecem na tabela do `index.qmd` como "sem anotações".
- As leituras principais são Llaudet e Imai (2022), em inglês, e Kellstedt e Whitten na tradução brasileira (`kellstedt2021fundamentos`, Blucher, 2021, a edição listada na ementa). As citações de Kellstedt e Whitten estão em português, e as páginas são dessa tradução.
- As notas de leitura de Llaudet e Imai, Cunningham e Shmueli estão em inglês; o resto está em português. Mantenha cada trecho na língua em que foi escrito.
- Trabalhos: `trabalhos/nivelamento` e `trabalhos/lista-1` a `lista-4`. Os dados das listas 2 a 4 estão no git (`data/` e o `.xlsx` da lista 4); os do nivelamento (`datasets/`, fora do git) não existem mais, por isso os blocos que dependem deles têm `eval: false` e as figuras vêm de `trabalhos/nivelamento/figs/`.
- `trabalhos/lista-3/_base_censo_2010.R` recria `belford_roxo.Rda` a partir do Censo 2010 (pacote `censobr`); não é executado no render.
- A lista 3 sorteia amostras sem cache: a reexecução dá números um pouco diferentes do PDF entregue a partir da seção "Distribuição amostral da média" (provavelmente porque o PDF foi gerado com `cache = TRUE`). O PDF é a versão oficial.
- O `lego-I.Rproj` fica na raiz, e não dentro de `trabalhos/`, para o `here::here()` achar a raiz do repositório a partir de qualquer página.
