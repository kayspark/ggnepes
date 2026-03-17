#' Nepes Discrete Color Scale
#'
#' @param theme Character. "light" (default) or "dark".
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{discrete_scale}}.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_nepes()
scale_color_nepes <- function(theme = "light", ...) {
  pal <- nepes_pal(theme)
  ggplot2::discrete_scale("colour", "nepes",
    function(n) pal[seq_len(n)], ...)
}

#' @rdname scale_color_nepes
#' @export
scale_colour_nepes <- scale_color_nepes

#' Nepes Discrete Fill Scale
#'
#' @inheritParams scale_color_nepes
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(class, fill = class)) +
#'   geom_bar() +
#'   scale_fill_nepes()
scale_fill_nepes <- function(theme = "light", ...) {
  pal <- nepes_pal(theme)
  ggplot2::discrete_scale("fill", "nepes",
    function(n) pal[seq_len(n)], ...)
}
