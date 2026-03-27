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
  ggplot2::discrete_scale(
    "colour",
    palette = function(n) pal[seq_len(n)],
    ...
  )
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
  ggplot2::discrete_scale(
    "fill",
    palette = function(n) pal[seq_len(n)],
    ...
  )
}

#' Nepes Highlight Fill Scale
#'
#' Convenience fill scale for binary highlight vs normal categories.
#'
#' @param highlight_color Character. Highlight color name from the Nepes base
#'   palette.
#' @param normal_color Character. Normal color name from the Nepes base palette.
#' @param theme Character. "light" (default) or "dark".
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_fill_manual}}.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = c("A", "B"), y = c(1, 2), type = c("normal", "highlight"))
#' ggplot(df, aes(x, y, fill = type)) +
#'   geom_col() +
#'   scale_fill_nepes_highlight()
scale_fill_nepes_highlight <- function(highlight_color = "red",
                                       normal_color = "blue",
                                       theme = "light",
                                       ...) {
  ggplot2::scale_fill_manual(
    values = c(
      highlight = nepes_color(highlight_color, theme = theme),
      normal = nepes_color(normal_color, theme = theme)
    ),
    ...
  )
}
