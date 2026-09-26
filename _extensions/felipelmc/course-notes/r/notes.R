# Course Notes · helpers de R para figuras no estilo do template.
#   source(here::here("_extensions/felipelmc/course-notes/r/notes.R"))
#   ggplot(...) + theme_notes() + scale_colour_notes()

cn_palette <- list(
  ink = "#0E1116", ink2 = "#444A54", ink3 = "#5F656E",
  line = "#DEDED8", line_strong = "#C9C9C1", bg = "#F6F6F3",
  accent = "#0A6F69", award = "#8A5A00", danger = "#A63D32",
  ramp = c("#5AC0AB", "#2E9D8E", "#187970", "#0E5652", "#053535")
)

# Cores qualitativas: acento, âmbar, rampa e cinzas (seguras em fundo claro)
cn_qual <- c("#0A6F69", "#8A5A00", "#5AC0AB", "#A63D32", "#0E5652", "#5F656E")

theme_notes <- function(base_size = 11, base_family = "") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 é necessário")
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = cn_palette$line, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = cn_palette$ink3),
      axis.title = ggplot2::element_text(colour = cn_palette$ink2),
      plot.title = ggplot2::element_text(colour = cn_palette$ink, face = "bold", size = base_size * 1.15),
      plot.subtitle = ggplot2::element_text(colour = cn_palette$ink2),
      plot.caption = ggplot2::element_text(colour = cn_palette$ink3, hjust = 0),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(colour = cn_palette$ink2),
      legend.text = ggplot2::element_text(colour = cn_palette$ink2),
      strip.text = ggplot2::element_text(colour = cn_palette$ink, face = "bold", hjust = 0)
    )
}

scale_colour_notes <- function(...) ggplot2::scale_colour_manual(values = cn_qual, ...)
scale_fill_notes <- function(...) ggplot2::scale_fill_manual(values = cn_qual, ...)
scale_colour_notes_c <- function(...) ggplot2::scale_colour_gradientn(colours = cn_palette$ramp, ...)
scale_fill_notes_c <- function(...) ggplot2::scale_fill_gradientn(colours = cn_palette$ramp, ...)
