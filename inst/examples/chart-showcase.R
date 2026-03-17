#!/usr/bin/env Rscript
# Nepes Chart Palette Showcase — generates PDF with all chart types
# Usage: Rscript chart-showcase.R
# Output: nepes-charts-light.pdf, nepes-charts-dark.pdf

library(ggplot2)
devtools::load_all(".")  # load ggnepes from source

set.seed(42)

# ── Sample data ──
n <- 100
ts_data <- data.frame(
  x = 1:n,
  power   = cumsum(rnorm(n, 0, 0.5)) + 50,
  temp    = cumsum(rnorm(n, 0, 0.3)) + 25,
  flow    = cumsum(rnorm(n, 0, 0.2)) + 10,
  voltage = cumsum(rnorm(n, 0, 0.4)) + 220,
  current = cumsum(rnorm(n, 0, 0.1)) + 5,
  gas     = cumsum(rnorm(n, 0, 0.15)) + 8
)

multi_data <- data.frame(
  x = rep(1:n, 6),
  y = c(ts_data$power, ts_data$temp * 2, ts_data$flow * 5,
        ts_data$voltage / 5, ts_data$current * 10, ts_data$gas * 6),
  series = rep(c("Power", "Temperature", "Flow", "Voltage", "Current", "Gas"), each = n)
)

cat_data <- data.frame(
  chamber = rep(paste0("CH", 1:6), each = 30),
  value = c(
    rnorm(30, 50, 5), rnorm(30, 52, 4), rnorm(30, 48, 6),
    rnorm(30, 51, 3), rnorm(30, 49, 7), rnorm(30, 53, 4)
  )
)

# SPC data
spc_n <- 50
spc_data <- data.frame(
  x = 1:spc_n,
  value = rnorm(spc_n, 50, 3)
)
spc_data$value[c(15, 38)] <- c(62, 41)  # violations
cl <- mean(spc_data$value)
ucl <- cl + 3 * sd(spc_data$value)
lcl <- cl - 3 * sd(spc_data$value)
usl <- 63
lsl <- 37
spc_colors <- nepes_spc()

# Scatter data
scatter_data <- data.frame(
  x = rnorm(120),
  y = rnorm(120),
  group = rep(paste0("Group ", 1:6), each = 20)
)

# ── Generate PDFs ──
generate_showcase <- function(theme_name) {
  is_dark <- theme_name == "dark"
  theme_fn <- if (is_dark) theme_nepes_dark else theme_nepes_light

  pdf(paste0("nepes-charts-", theme_name, ".pdf"), width = 10, height = 7)

  # 1. Multi-series time series (6 colors)
  p1 <- ggplot(multi_data, aes(x, y, color = series)) +
    geom_line(linewidth = 0.8) +
    scale_color_nepes(theme = theme_name) +
    theme_fn() +
    labs(title = "Multi-Series Time Series (6 colors)",
         subtitle = paste("Nepes", theme_name, "palette"),
         x = "Sample", y = "Value", color = "Parameter")
  print(p1)

  # 2. Box plot (6 categories)
  p2 <- ggplot(cat_data, aes(chamber, value, fill = chamber)) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_nepes(theme = theme_name) +
    theme_fn() +
    labs(title = "Chamber Comparison (Box Plot)",
         x = "Chamber", y = "Measurement")
  print(p2)

  # 3. Scatter plot (6 groups)
  p3 <- ggplot(scatter_data, aes(x, y, color = group)) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_nepes(theme = theme_name) +
    theme_fn() +
    labs(title = "PCA-style Scatter (6 groups)",
         x = "PC1", y = "PC2", color = "Cluster")
  print(p3)

  # 4. Bar chart
  bar_data <- data.frame(
    category = paste0("Step ", 1:6),
    value = c(85, 72, 93, 68, 88, 76)
  )
  p4 <- ggplot(bar_data, aes(category, value, fill = category)) +
    geom_col(show.legend = FALSE) +
    scale_fill_nepes(theme = theme_name) +
    theme_fn() +
    labs(title = "Process Step Yield", x = "Step", y = "Yield (%)")
  print(p4)

  # 5. SPC Control Chart
  violation_idx <- which(spc_data$value > ucl | spc_data$value < lcl)
  p5 <- ggplot(spc_data, aes(x, value)) +
    geom_line(color = spc_colors["data_points"], linewidth = 0.5) +
    geom_point(color = spc_colors["data_points"], size = 1.5) +
    geom_hline(yintercept = cl, color = spc_colors["center_line"], linewidth = 1) +
    geom_hline(yintercept = c(ucl, lcl), color = spc_colors["control_limit"],
               linetype = "dashed", linewidth = 0.7) +
    geom_hline(yintercept = c(usl, lsl), color = spc_colors["spec_limit"],
               linetype = "dotted", linewidth = 0.7) +
    geom_point(data = spc_data[violation_idx, ], aes(x, value),
               color = spc_colors["violation"], size = 3, shape = 18) +
    theme_fn() +
    labs(title = "SPC X-bar Control Chart",
         subtitle = "CL (blue), UCL/LCL (gray), USL/LSL (orange), Violations (red)",
         x = "Subgroup", y = "Value")
  print(p5)

  # 6. Histogram with density
  p6 <- ggplot(cat_data, aes(value, fill = chamber)) +
    geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
    scale_fill_nepes(theme = theme_name) +
    theme_fn() +
    labs(title = "Distribution Overlay (6 chambers)",
         x = "Value", y = "Count", fill = "Chamber")
  print(p6)

  dev.off()
  cat("  Generated nepes-charts-", theme_name, ".pdf (6 charts)\n", sep = "")
}

generate_showcase("light")
generate_showcase("dark")
cat("Done!\n")
