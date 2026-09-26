-- Course Notes: filtro principal (roda em pre-ast).
--   * carrega as fontes auto-hospedadas (uma dependência HTML só, copiada para site_libs);
--   * calcula `aula-label` ("03") a partir de `aula:` para o bloco de título;
--   * ::: nota-ia       -> callout recolhível rotulado com a ferramenta de IA usada;
--   * ::: prova         -> <details> no HTML, ambiente proof nos demais formatos;
--   * ### @chave ...    -> cabeçalho de leitura (classe .leitura).

local function is_html()
  return quarto.doc.is_format("html")
end

local function stringify(x)
  return pandoc.utils.stringify(x)
end

function Meta(meta)
  if is_html() then
    quarto.doc.add_html_dependency({
      name = "course-notes-fonts",
      version = "1.0.0",
      stylesheets = { "fonts/fonts.css" },
      resources = {
        "fonts/geist-latin-wght-normal.woff2",
        "fonts/geist-latin-ext-wght-normal.woff2",
        "fonts/geist-mono-latin-wght-normal.woff2",
        "fonts/geist-mono-latin-ext-wght-normal.woff2",
        "fonts/newsreader-latin-opsz-normal.woff2",
        "fonts/newsreader-latin-opsz-italic.woff2",
        "fonts/newsreader-latin-ext-opsz-normal.woff2",
        "fonts/newsreader-latin-ext-opsz-italic.woff2",
      },
    })
  end
  if meta.aula ~= nil then
    local raw = stringify(meta.aula)
    local n = tonumber(raw)
    local label = n and string.format("%02d", n) or raw
    meta["aula-label"] = pandoc.MetaString(label)
  end
  return meta
end

local function nota_ia(div)
  local attrs = div.attributes
  local ferramenta = attrs["ferramenta"] or "Claude"
  attrs["ferramenta"] = nil
  if attrs["title"] == nil then
    attrs["title"] = "Nota explicativa gerada com auxílio do " .. ferramenta
  end
  if attrs["collapse"] == nil then attrs["collapse"] = "true" end
  if attrs["icon"] == nil then attrs["icon"] = "false" end
  if attrs["appearance"] == nil then attrs["appearance"] = "default" end
  div.classes:insert(1, "callout-note")
  return div
end

local function prova(div)
  local titulo = div.attributes["title"] or "Prova"
  if is_html() then
    local aberta = div.attributes["aberta"] == "true" and " open" or ""
    local out = pandoc.List({
      pandoc.RawBlock("html", '<details class="cn-prova"' .. aberta .. '><summary>' .. titulo .. '</summary>'),
    })
    out:extend(div.content)
    out:insert(pandoc.RawBlock("html", '</details>'))
    return out
  end
  div.classes = pandoc.List({ "proof" })
  div.attributes["aberta"] = nil
  return div
end

function Div(div)
  if div.classes:includes("nota-ia") then
    return nota_ia(div)
  end
  if div.classes:includes("prova") then
    return prova(div)
  end
end

function Header(h)
  if (h.level == 3 or h.level == 4) and #h.content > 0 and h.content[1].t == "Cite" then
    if not h.classes:includes("leitura") then
      h.classes:insert("leitura")
    end
    return h
  end
end

return {
  { Meta = Meta },
  { Div = Div, Header = Header },
}
