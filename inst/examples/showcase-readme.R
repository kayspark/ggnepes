#!/usr/bin/env Rscript
# README Screenshot — Light vs Dark side-by-side
# Picks 6 representative geom charts, renders each in both themes,
# and combines into a single image for the README.
#
# Usage: cd ggnepes && Rscript inst/examples/showcase-readme.R
# Output: docs/light.png, docs/dark.png

library(ggplot2)
devtools::load_all(".")

set.seed(42)

# ── Sample data ──
data(iris)
data(mpg)
data(diamonds)
data(faithful)

n <- 100
ts_df <- data.frame(
  x = rep(1:n, 6),
  y = c(cumsum(rnorm(n, 0, 0.5)) + 50,
        cumsum(rnorm(n, 0, 0.3)) + 25,
        cumsum(rnorm(n, 0, 0.2)) + 10,
        cumsum(rnorm(n, 0, 0.4)) + 44,
        cumsum(rnorm(n, 0, 0.1)) + 5,
        cumsum(rnorm(n, 0, 0.15)) + 8),
  series = rep(c("Power", "Temp", "Flow", "Voltage", "Current", "Gas"), each = n)
)

spc_n <- 50
spc_df <- data.frame(x = 1:spc_n, value = rnorm(spc_n, 50, 3))
spc_df$value[c(15, 38)] <- c(62, 41)
cl  <- mean(spc_df$value)
ucl <- cl + 3 * sd(spc_df$value)
lcl <- cl - 3 * sd(spc_df$value)

huron_df <- data.frame(year = 1875:1972, level = as.numeric(LakeHuron))

err_df <- data.frame(
  treatment = rep(c("T1", "T2", "T3", "T4"), 2),
  group     = rep(c("Group A", "Group B"), each = 4),
  response  = c(5.2, 4.1, 3.8, 4.5, 3.9, 3.2, 4.3, 3.7),
  lower     = c(4.5, 3.4, 3.0, 3.8, 3.2, 2.5, 3.6, 3.0),
  upper     = c(5.9, 4.8, 4.6, 5.2, 4.6, 3.9, 5.0, 4.4)
)

# ── Chart builders (return plot objects, no theme yet) ──
make_plots <- function(theme_name) {
  theme_fn <- if (theme_name == "dark") theme_nepes_dark else theme_nepes_light
  pal <- nepes_pal(theme_name, n = 6)
  spc_col <- nepes_spc(theme_name)
  fg <- if (theme_name == "dark") "#DCD8D4" else "#1C1C1E"
  bs <- 8

  plots <- list()

  # 1. geom_line — multi-series time series
  plots[[1]] <- ggplot(ts_df, aes(x, y, color = series)) +
    geom_line(linewidth = 0.5) +
    scale_color_nepes(theme = theme_name) + theme_fn(base_size = bs) +
    labs(title = "geom_line", x = "Sample", y = "Value")

  # 2. geom_boxplot + geom_jitter
  plots[[2]] <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.8) +
    geom_jitter(width = 0.12, size = 0.5, alpha = 0.3) +
    scale_fill_nepes(theme = theme_name) + theme_fn(base_size = bs) +
    labs(title = "geom_boxplot + jitter", x = NULL, y = "Sepal Length")

  # 3. geom_density
  plots[[3]] <- ggplot(iris, aes(Petal.Width, fill = Species)) +
    geom_density(alpha = 0.5) +
    scale_fill_nepes(theme = theme_name) + theme_fn(base_size = bs) +
    labs(title = "geom_density", x = "Petal Width", y = "Density")

  # 4. SPC control chart (geom_line + geom_point + geom_hline)
  viol_idx <- which(spc_df$value > ucl | spc_df$value < lcl)
  plots[[4]] <- ggplot(spc_df, aes(x, value)) +
    geom_line(color = spc_col["data_points"], linewidth = 0.4) +
    geom_point(color = spc_col["data_points"], size = 0.8) +
    geom_hline(yintercept = cl, color = spc_col["center_line"], linewidth = 0.8) +
    geom_hline(yintercept = c(ucl, lcl), color = spc_col["control_limit"],
               linetype = "dashed", linewidth = 0.5) +
    {if (length(viol_idx) > 0)
      geom_point(data = spc_df[viol_idx, ], color = spc_col["violation"],
                 size = 2, shape = 18)} +
    theme_fn(base_size = bs) +
    labs(title = "SPC X-bar Chart", x = "Subgroup", y = "Value")

  # 5. geom_ribbon (Lake Huron)
  plots[[5]] <- ggplot(huron_df, aes(year, level)) +
    geom_ribbon(aes(ymin = level - 1, ymax = level + 1),
                fill = pal[1], alpha = 0.3) +
    geom_line(color = pal[1], linewidth = 0.6) +
    theme_fn(base_size = bs) +
    labs(title = "geom_ribbon", x = "Year", y = "Level (ft)")

  # 6. geom_errorbar + geom_point
  plots[[6]] <- ggplot(err_df, aes(treatment, response, color = group)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.3), width = 0.2) +
    geom_point(position = position_dodge(width = 0.3), size = 1.5) +
    scale_color_nepes(theme = theme_name) + theme_fn(base_size = bs) +
    labs(title = "geom_errorbar", x = "Treatment", y = "Response")

  # 7. geom_smooth
  plots[[7]] <- ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
    geom_point(alpha = 0.4, size = 0.8) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    scale_color_nepes(theme = theme_name) + theme_fn(base_size = bs) +
    labs(title = "geom_smooth (lm)", x = "Displacement", y = "Highway MPG",
         color = "Cylinders")

  # 8. geom_density_2d_filled
  plots[[8]] <- ggplot(faithful, aes(eruptions, waiting)) +
    geom_density_2d_filled(alpha = 0.7, show.legend = FALSE) +
    theme_fn(base_size = bs) +
    labs(title = "geom_density_2d_filled", x = "Eruptions", y = "Waiting")

  # 9. geom_histogram
  plots[[9]] <- ggplot(diamonds[diamonds$price < 8000, ],
                       aes(price, fill = cut)) +
    geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
    scale_fill_nepes(theme = theme_name) + theme_fn(base_size = bs) +
    labs(title = "geom_histogram", x = "Price ($)", y = "Count")

  plots
}

dir.create("docs", showWarnings = FALSE)

for (theme_name in c("dark", "light")) {
  is_dark <- theme_name == "dark"
  bg <- if (is_dark) "#1E1C1A" else "white"
  fg <- if (is_dark) "#DCD8D4" else "#1C1C1E"

  plots <- make_plots(theme_name)

  combined <- gridExtra::arrangeGrob(
    grobs = plots, ncol = 3,
    top = grid::textGrob(
      paste0("ggnepes \u2014 ", tools::toTitleCase(theme_name), " Theme"),
      gp = grid::gpar(fontsize = 14, fontface = "bold", col = fg)
    )
  )

  png(paste0("docs/", theme_name, ".png"), width = 1800, height = 1600,
      res = 150, bg = bg)
  grid::grid.draw(combined)
  dev.off()
  cat("  Generated docs/", theme_name, ".png\n", sep = "")
}

cat("Done!\n")
