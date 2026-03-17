#!/usr/bin/env Rscript
# Nepes Chart Showcase — SECOM semiconductor manufacturing data
#
# Data source: UCI Machine Learning Repository — SECOM Dataset
#   https://archive.ics.uci.edu/ml/datasets/SECOM
#   Semi-conductor manufacturing process data (1567 samples, 590 features).
#   This is PUBLIC SAMPLE DATA, not production data.
#
# Usage: cd ggnepes && R --vanilla -e "source('inst/examples/showcase-secom.R')"
# Output: docs/light.png, docs/dark.png

library(ggplot2)
devtools::load_all(".")

data_dir <- "../../fdc-analytics/data/secom"
raw <- read.table(file.path(data_dir, "secom.data"), header = FALSE)
lab <- read.table(file.path(data_dir, "secom_labels.data"), header = FALSE)

# Select 6 features with good variance
feat_idx <- c(1, 2, 3, 4, 6, 7)
feat_names <- c("Sensor A", "Sensor B", "Sensor C", "Sensor D", "Sensor E", "Sensor F")
features <- raw[, feat_idx]
colnames(features) <- feat_names
features$label <- ifelse(lab$V1 == -1, "Pass", "Fail")

# Replace NA with column mean
for (col in feat_names) {
  features[[col]][is.na(features[[col]])] <- mean(features[[col]], na.rm = TRUE)
}

dir.create("docs", showWarnings = FALSE)

generate_showcase <- function(theme_name) {
  is_dark <- theme_name == "dark"
  theme_fn <- if (is_dark) theme_nepes_dark else theme_nepes_light
  spc <- nepes_spc(theme_name)
  bg <- if (is_dark) "#1E1C1A" else "white"
  sub_col <- if (is_dark) "#8A9199" else "#7A7A84"

  plots <- list()

  # 1. Multi-sensor time series (first 200)
  ts_df <- data.frame(
    sample = rep(1:200, 6),
    value = unlist(lapply(feat_names, function(n) {
      v <- features[[n]][1:200]
      (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
    })),
    sensor = rep(feat_names, each = 200)
  )
  plots[[1]] <- ggplot(ts_df, aes(sample, value, color = sensor)) +
    geom_line(linewidth = 0.5, alpha = 0.85) +
    scale_color_nepes(theme = theme_name) + theme_fn(base_size = 9) +
    labs(title = "Multi-Sensor Time Series", x = "Sample", y = "Normalized")

  # 2. Box plot
  box_df <- tidyr::pivot_longer(features[features$label == "Pass", feat_names],
                                 cols = everything(), names_to = "sensor", values_to = "value")
  plots[[2]] <- ggplot(box_df, aes(sensor, value, fill = sensor)) +
    geom_boxplot(show.legend = FALSE, outlier.size = 0.5) +
    scale_fill_nepes(theme = theme_name) + theme_fn(base_size = 9) +
    labs(title = "Feature Distribution (Pass)", x = NULL, y = "Value") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  # 3. PCA scatter
  pca_cols <- feat_names[sapply(feat_names, function(n) sd(features[[n]], na.rm = TRUE) > 0)]
  pca_res <- prcomp(features[, pca_cols], scale. = TRUE)
  pca_df <- data.frame(PC1 = pca_res$x[, 1], PC2 = pca_res$x[, 2], label = features$label)
  pct <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)
  plots[[3]] <- ggplot(pca_df, aes(PC1, PC2, color = label)) +
    geom_point(size = 1, alpha = 0.5) +
    scale_color_manual(values = c("Pass" = nepes_pal(theme_name)[1],
                                   "Fail" = nepes_pal(theme_name)[4])) +
    theme_fn(base_size = 9) +
    labs(title = "PCA — Pass vs Fail",
         x = paste0("PC1 (", pct[1], "%)"), y = paste0("PC2 (", pct[2], "%)"))

  # 4. Bar chart — std dev
  std_df <- data.frame(
    sensor = feat_names,
    std = sapply(feat_names, function(n) sd(features[[n]], na.rm = TRUE))
  )
  std_df$sensor <- factor(std_df$sensor, levels = feat_names)
  plots[[4]] <- ggplot(std_df, aes(sensor, std, fill = sensor)) +
    geom_col(show.legend = FALSE) +
    scale_fill_nepes(theme = theme_name) + theme_fn(base_size = 9) +
    labs(title = "Feature Std Dev", x = NULL, y = "Std Dev") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  # 5. SPC control chart
  sensor_a <- features[["Sensor A"]][1:100]
  cl_val <- mean(sensor_a, na.rm = TRUE)
  sigma <- sd(sensor_a, na.rm = TRUE)
  ucl <- cl_val + 3 * sigma
  lcl <- cl_val - 3 * sigma
  spc_df <- data.frame(x = 1:100, value = sensor_a)
  viol <- spc_df[spc_df$value > ucl | spc_df$value < lcl, ]

  plots[[5]] <- ggplot(spc_df, aes(x, value)) +
    geom_line(color = spc["data_points"], linewidth = 0.5) +
    geom_point(color = spc["data_points"], size = 1) +
    geom_hline(yintercept = cl_val, color = spc["center_line"], linewidth = 1) +
    geom_hline(yintercept = c(ucl, lcl), color = spc["control_limit"], linetype = "dashed") +
    {if (nrow(viol) > 0) geom_point(data = viol, color = spc["violation"], size = 2.5, shape = 18)} +
    theme_fn(base_size = 9) +
    labs(title = "SPC — Sensor A", x = "Sample", y = "Value")

  # 6. Histogram pass vs fail
  hist_df <- data.frame(value = features[["Sensor A"]], label = features$label)
  plots[[6]] <- ggplot(hist_df, aes(value, fill = label)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    scale_fill_manual(values = c("Pass" = nepes_pal(theme_name)[1],
                                  "Fail" = nepes_pal(theme_name)[4])) +
    theme_fn(base_size = 9) +
    labs(title = "Sensor A — Pass vs Fail", x = "Value", y = "Count")

  # Combine into 2x3 grid
  combined <- gridExtra::arrangeGrob(
    grobs = plots, ncol = 3,
    top = grid::textGrob(
      paste0("Nepes ", tools::toTitleCase(theme_name), " \u2014 SECOM Semiconductor Data\n",
             "Data: UCI ML Repository SECOM Dataset (public sample, not production)"),
      gp = grid::gpar(fontsize = 13, fontface = "bold",
                       col = if (is_dark) "#DCD8D4" else "#1C1C1E")
    )
  )

  png(paste0("docs/", theme_name, ".png"), width = 1600, height = 1100, res = 150,
      bg = bg)
  grid::grid.draw(combined)
  dev.off()
  cat("  Generated docs/", theme_name, ".png\n", sep = "")
}

generate_showcase("light")
generate_showcase("dark")
cat("Done!\n")
