-- {{< slides url="caminho/ou/URL" title="Título" >}}
-- Incorpora uma apresentação (revealjs ou PDF) num quadro 16:9, com link para abrir em tela cheia.
return {
  ["slides"] = function(args, kwargs, meta)
    local url = pandoc.utils.stringify(kwargs["url"] or args[1] or "")
    local title = pandoc.utils.stringify(kwargs["title"] or "Slides")
    if url == "" then
      return pandoc.Null()
    end
    if quarto.doc.is_format("html") then
      local html = string.format([[
<figure class="cn-slides">
  <div class="cn-slides-frame">
    <iframe src="%s" title="%s" loading="lazy" allowfullscreen></iframe>
  </div>
  <figcaption><a href="%s" target="_blank" rel="noopener">Abrir os slides em tela cheia ↗</a></figcaption>
</figure>]], url, title, url)
      return pandoc.RawBlock("html", html)
    end
    return pandoc.Para({ pandoc.Link(title, url) })
  end,
}
