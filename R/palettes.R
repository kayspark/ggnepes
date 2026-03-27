#' Nepes Color Palette
#'
#' Returns a character vector of hex colors from the Nepes corporate palette.
#'
#' @param theme Character. Either "light" (default) or "dark".
#' @param n Integer. Number of colors to return (default: 12).
#' @return A character vector of hex color strings.
#' @export
#' @examples
#' nepes_pal()
#' nepes_pal("dark", n = 6)
nepes_pal <- function(theme = "light", n = 12) {
  pal <- .nepes_palette(theme)
  if (n > length(pal)) n <- length(pal)
  pal[seq_len(n)]
}

#' Nepes Named Color
#'
#' Returns a single named color from the Nepes base palette.
#'
#' @param name Character. One of "blue", "orange", "green", "red", "teal",
#'   or "purple".
#' @param theme Character. Either "light" (default) or "dark".
#' @return A single hex color string.
#' @export
#' @examples
#' nepes_color("red")
#' nepes_color("red", theme = "dark")
nepes_color <- function(name, theme = "light") {
  pal <- .nepes_palette(theme)

  if (length(name) != 1 || is.na(name) || !name %in% .nepes_base_names) {
    stop(
      "`name` must be one of: ",
      paste(.nepes_base_names, collapse = ", "),
      call. = FALSE
    )
  }

  pal[[name]]
}

#' Nepes SPC Palette
#'
#' Returns a named character vector of colors for SPC (Statistical Process
#' Control) charts: center line, data points, control limits, spec limits,
#' and violations.
#'
#' @param theme Character. Either "light" (default) or "dark".
#' @return A named character vector.
#' @export
#' @examples
#' nepes_spc()
#' nepes_spc("dark")
nepes_spc <- function(theme = "light") {
  if (theme == "dark") .nepes_spc_dark else .nepes_spc_light
}

.nepes_palette <- function(theme) {
  if (!theme %in% c("light", "dark")) {
    stop("`theme` must be either \"light\" or \"dark\".", call. = FALSE)
  }

  if (theme == "dark") .nepes_dark_12 else .nepes_light_12
}

.nepes_base_names <- c("blue", "orange", "green", "red", "teal", "purple")
.nepes_palette_names <- c(
  .nepes_base_names,
  "blue2", "orange2", "green2", "red2", "teal2", "purple2"
)

# ── Light theme (12 colors: 6 bases + 6 midtones) ──
.nepes_light_12 <- c(
  "#23438E", "#C25609", "#017939", "#C4181F", "#2D7A82", "#873D8E",
  "#6278AB", "#A7693E", "#017939", "#B75B5F", "#56898E", "#96699A"
)
names(.nepes_light_12) <- .nepes_palette_names

# ── Dark theme (12 colors: 6 bases + 6 midtones) ──
.nepes_dark_12 <- c(
  "#5C8CFF", "#FEA413", "#3DDC84", "#FF5C5C", "#3A9BA5", "#A274C3",
  "#6B8AD8", "#FFBD4A", "#6BCF70", "#E85B61", "#5CBDC7", "#BB8EDA"
)
names(.nepes_dark_12) <- .nepes_palette_names

# ── SPC palettes ──
.nepes_spc_light <- c(
  center_line   = "#23438E",
  data_points   = "#5A7EB0",
  control_limit = "#6A6A6A",
  spec_limit    = "#C25609",
  violation     = "#C4181F"
)

.nepes_spc_dark <- c(
  center_line   = "#5C8CFF",
  data_points   = "#6B8AD8",
  control_limit = "#8A9199",
  spec_limit    = "#FEA413",
  violation     = "#FF5C5C"
)
