#' Nepes Light Theme for ggplot2
#'
#' A clean, minimal theme with white background using Nepes palette colors.
#'
#' @param base_size Base font size (default: 11).
#' @param base_family Font family (default: system sans-serif).
#' @return A ggplot2 theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   theme_nepes_light()
theme_nepes_light <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major   = ggplot2::element_line(color = "#E7E6E6"),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(color = "#3A3A3E"),
      axis.title         = ggplot2::element_text(color = "#1C1C1E"),
      plot.title         = ggplot2::element_text(color = "#1C1C1E", face = "bold"),
      plot.subtitle      = ggplot2::element_text(color = "#5A5A64"),
      legend.background  = ggplot2::element_rect(fill = "white", color = NA),
      legend.key         = ggplot2::element_rect(fill = "white", color = NA),
      strip.background   = ggplot2::element_rect(fill = "#F0F0F0", color = NA),
      strip.text         = ggplot2::element_text(color = "#1C1C1E")
    )
}

#' Nepes Dark Theme for ggplot2
#'
#' A minimal theme with warm dark background using Nepes palette colors.
#'
#' @inheritParams theme_nepes_light
#' @return A ggplot2 theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   theme_nepes_dark()
theme_nepes_dark <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "#1E1C1A", color = NA),
      panel.background   = ggplot2::element_rect(fill = "#1E1C1A", color = NA),
      panel.grid.major   = ggplot2::element_line(color = "#3E3A38"),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(color = "#B6B0AC"),
      axis.title         = ggplot2::element_text(color = "#DCD8D4"),
      plot.title         = ggplot2::element_text(color = "#DCD8D4", face = "bold"),
      plot.subtitle      = ggplot2::element_text(color = "#8A9199"),
      legend.background  = ggplot2::element_rect(fill = "#2E2C2A", color = NA),
      legend.key         = ggplot2::element_rect(fill = "#2E2C2A", color = NA),
      strip.background   = ggplot2::element_rect(fill = "#2E2C2A", color = NA),
      strip.text         = ggplot2::element_text(color = "#DCD8D4")
    )
}
