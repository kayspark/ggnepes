#!/usr/bin/env Rscript
# All 53 ggplot2 Geom Functions — Nepes Theme Showcase
# Based on: https://youtu.be/ZrkjRwsnj6Y (The Data Digest)
# Usage: cd ggnepes && Rscript inst/examples/geom53-showcase.R
# Output: geom53-dark.pdf, geom53-light.pdf (one chart per page)

library(ggplot2)
devtools::load_all(".")

set.seed(42)

# ══════════════════════════════════════════════════════════════════════
# Sample Data
# ══════════════════════════════════════════════════════════════════════

# -- iris-like data for scatter/box/violin --
data(iris)

# -- mpg for bars, smooth, rug --
data(mpg)

# -- diamonds for histogram, hex, bin2d --
data(diamonds)

# -- faithful for density_2d --
data(faithful)

# -- line data (6 points) --
line_df <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 3, 1, 4, 2, 3))

# -- multi-series time series --
n <- 100
ts_df <- data.frame(
  x    = rep(1:n, 6),
  y    = c(cumsum(rnorm(n, 0, 0.5)) + 50,
           cumsum(rnorm(n, 0, 0.3)) + 25,
           cumsum(rnorm(n, 0, 0.2)) + 10,
           cumsum(rnorm(n, 0, 0.4)) + 44,
           cumsum(rnorm(n, 0, 0.1)) + 5,
           cumsum(rnorm(n, 0, 0.15)) + 8),
  series = rep(c("Power", "Temp", "Flow", "Voltage", "Current", "Gas"), each = n)
)

# -- area data --
area_df <- data.frame(x = 1:20, y = cumsum(runif(20, 0, 2)))

# -- stacked area data --
stacked_area <- data.frame(
  time  = rep(1:20, 5),
  group = rep(LETTERS[1:5], each = 20),
  value = c(cumsum(runif(20, 0, 1)),
            cumsum(runif(20, 0, 1.2)),
            cumsum(runif(20, 0, 0.8)),
            cumsum(runif(20, 0, 1.1)),
            cumsum(runif(20, 0, 0.9)))
)

# -- error/uncertainty data --
err_df <- data.frame(
  treatment = rep(c("T1", "T2", "T3", "T4"), 2),
  group     = rep(c("Group A", "Group B"), each = 4),
  response  = c(5.2, 4.1, 3.8, 4.5, 3.9, 3.2, 4.3, 3.7),
  lower     = c(4.5, 3.4, 3.0, 3.8, 3.2, 2.5, 3.6, 3.0),
  upper     = c(5.9, 4.8, 4.6, 5.2, 4.6, 3.9, 5.0, 4.4)
)

# -- SPC data --
spc_n <- 50
spc_df <- data.frame(x = 1:spc_n, value = rnorm(spc_n, 50, 3))
spc_df$value[c(15, 38)] <- c(62, 41)
cl  <- mean(spc_df$value)
ucl <- cl + 3 * sd(spc_df$value)
lcl <- cl - 3 * sd(spc_df$value)

# -- scatter groups --
scatter_df <- data.frame(
  x = rnorm(120), y = rnorm(120),
  group = rep(paste0("G", 1:6), each = 20)
)

# -- segment data --
seg_df <- data.frame(
  x = c(1, 3, 5), xend = c(2, 4, 6),
  y = c(1, 3, 2), yend = c(3, 1, 4),
  label = c("A", "B", "C")
)

# -- curve data --
curve_df <- data.frame(
  x = c(1, 3), xend = c(4, 5),
  y = c(1, 4), yend = c(4, 1)
)

# -- rect data --
rect_df <- data.frame(
  xmin = c(1, 3, 5), xmax = c(2, 4, 6),
  ymin = c(1, 2, 0), ymax = c(3, 5, 2),
  group = c("A", "B", "C")
)

# -- tile data --
tile_df <- expand.grid(x = 1:5, y = 1:5)
tile_df$z <- runif(25)

# -- spoke data --
spoke_df <- expand.grid(x = 1:8, y = 1:8)
spoke_df$angle <- runif(64, 0, 2 * pi)
spoke_df$speed <- runif(64, 0.2, 1)

# -- density raster (pre-computed) --
raster_df <- expand.grid(x = seq(-3, 3, length.out = 50),
                         y = seq(-3, 3, length.out = 50))
raster_df$z <- with(raster_df, dnorm(x) * dnorm(y))

# -- contour data --
contour_df <- expand.grid(x = seq(-3, 3, length.out = 80),
                          y = seq(-3, 3, length.out = 80))
contour_df$z <- with(contour_df, dnorm(x) * dnorm(y))

# -- qq data --
qq_df <- data.frame(y = rt(200, df = 5))

# -- map data --
us_map  <- map_data("state")
us_data <- data.frame(
  region = unique(us_map$region),
  value  = runif(length(unique(us_map$region)), 0, 100)
)

# -- ribbon data (Lake Huron levels) --
huron_df <- data.frame(
  year  = 1875:1972,
  level = as.numeric(LakeHuron)
)

# -- function plot data --
func_df <- data.frame(x = rnorm(100))

# -- label data (subset of mtcars) --
mtcars$model <- rownames(mtcars)
label_df <- head(mtcars, 8)

# ══════════════════════════════════════════════════════════════════════
# Showcase Generator
# ══════════════════════════════════════════════════════════════════════

generate_showcase <- function(theme_name) {
  is_dark  <- theme_name == "dark"
  theme_fn <- if (is_dark) theme_nepes_dark else theme_nepes_light
  pal      <- nepes_pal(theme_name, n = 6)
  spc_col  <- nepes_spc(theme_name)
  bg       <- if (is_dark) "#1E1C1A" else "white"
  fg       <- if (is_dark) "#DCD8D4" else "#1C1C1E"
  muted    <- if (is_dark) "#8A9199" else "#5A5A64"

  pdf(paste0("geom53-", theme_name, ".pdf"), width = 10, height = 7, bg = bg)

  pp <- function(p) print(p)

  # ────────────────────────────────────────────────────────────────
  # SECTION 1: Scatter & Points
  # ────────────────────────────────────────────────────────────────

  # 1. geom_point
  pp(ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
       geom_point(size = 2, alpha = 0.7) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "1. geom_point — Scatter Plot",
            subtitle = "Iris: sepal length vs width by species"))

  # 2. geom_jitter
  pp(ggplot(iris, aes(Species, Petal.Width, color = Species)) +
       geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "2. geom_jitter — Jittered Points",
            subtitle = "Petal width by species with random noise"))

  # 3. geom_count
  pp(ggplot(mpg, aes(cty, hwy)) +
       geom_count(color = pal[1], alpha = 0.6) +
       theme_fn() +
       labs(title = "3. geom_count — Sized by Overlap Count",
            subtitle = "City vs highway mileage"))

  # 4. geom_rug
  pp(ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
       geom_point(size = 1.5, alpha = 0.6) +
       geom_rug(alpha = 0.4, sides = "bl") +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "4. geom_rug — Marginal Tick Marks",
            subtitle = "Rug on bottom and left axes"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 2: Box / Violin / Dot
  # ────────────────────────────────────────────────────────────────

  # 5. geom_boxplot
  pp(ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
       geom_boxplot(outlier.shape = NA, alpha = 0.8) +
       geom_jitter(width = 0.15, size = 1, alpha = 0.3) +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "5. geom_boxplot — Box Plot with Jitter",
            subtitle = "Sepal length by species"))

  # 6. geom_violin
  pp(ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
       geom_violin(alpha = 0.7) +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "6. geom_violin — Violin Plot",
            subtitle = "Distribution shape of sepal length"))

  # 7. geom_dotplot
  pp(ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
       geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.8) +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "7. geom_dotplot — Dot Plot",
            subtitle = "Sepal width stacked by category"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 3: Bars & Columns
  # ────────────────────────────────────────────────────────────────

  # 8. geom_bar
  pp(ggplot(mpg, aes(class, fill = class)) +
       geom_bar() +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "8. geom_bar — Bar Chart (auto count)",
            subtitle = "Car class frequency in mpg") +
       theme(axis.text.x = element_text(angle = 30, hjust = 1)))

  # 9. geom_col
  bar_vals <- data.frame(
    step  = paste0("Step ", 1:6),
    yield = c(85, 72, 93, 68, 88, 76)
  )
  pp(ggplot(bar_vals, aes(step, yield, fill = step)) +
       geom_col(show.legend = FALSE) +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "9. geom_col — Column Chart (pre-computed)",
            subtitle = "Process step yield percentages"))

  # 10. geom_histogram
  pp(ggplot(diamonds, aes(price)) +
       geom_histogram(bins = 50, fill = pal[1], color = fg, linewidth = 0.2) +
       theme_fn() +
       labs(title = "10. geom_histogram — Price Distribution",
            subtitle = "Diamond prices (53K observations)"))

  # 11. geom_freqpoly
  pp(ggplot(diamonds, aes(price, color = cut)) +
       geom_freqpoly(bins = 40, linewidth = 0.8) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "11. geom_freqpoly — Frequency Polygon",
            subtitle = "Diamond price by cut quality"))

  # 12. geom_density
  pp(ggplot(iris, aes(Petal.Width, fill = Species)) +
       geom_density(alpha = 0.5) +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "12. geom_density — Density Distribution",
            subtitle = "Petal width by species"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 4: Lines & Paths
  # ────────────────────────────────────────────────────────────────

  # 13. geom_line
  pp(ggplot(ts_df, aes(x, y, color = series)) +
       geom_line(linewidth = 0.7) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "13. geom_line — Multi-Series Time Series",
            subtitle = "Six sensor readings"))

  # 14. geom_step
  pp(ggplot(line_df, aes(x, y)) +
       geom_step(color = pal[1], linewidth = 1) +
       geom_point(color = pal[4], size = 3) +
       theme_fn() +
       labs(title = "14. geom_step — Step Function",
            subtitle = "Horizontal-then-vertical connections"))

  # 15. geom_path
  pp(ggplot(line_df, aes(x, y)) +
       geom_path(color = pal[3], linewidth = 1) +
       geom_point(color = pal[4], size = 3) +
       geom_text(aes(label = seq_len(nrow(line_df))),
                 vjust = -1, color = fg, size = 4) +
       theme_fn() +
       labs(title = "15. geom_path — Path in Data Order",
            subtitle = "Points connected in row order, not sorted by x"))

  # 16. geom_area
  pp(ggplot(area_df, aes(x, y)) +
       geom_area(fill = pal[1], alpha = 0.6) +
       geom_line(color = pal[1], linewidth = 0.8) +
       theme_fn() +
       labs(title = "16. geom_area — Filled Area",
            subtitle = "Cumulative values with shaded region"))

  # 17. geom_area (stacked)
  pp(ggplot(stacked_area, aes(time, value, fill = group)) +
       geom_area(alpha = 0.7) +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "17. geom_area — Stacked Area Chart",
            subtitle = "Five groups stacked over time"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 5: Reference Lines & Segments
  # ────────────────────────────────────────────────────────────────

  # 18. geom_hline
  pp(ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
       geom_point(alpha = 0.6) +
       geom_hline(yintercept = mean(mpg$hwy), linetype = "dashed",
                  color = pal[2], linewidth = 0.8) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "18. geom_hline — Horizontal Reference Line",
            subtitle = "Dashed line at mean highway mileage"))

  # 19. geom_vline
  pp(ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
       geom_point(alpha = 0.6) +
       geom_vline(xintercept = c(2.6, 3.5), linetype = "dashed",
                  color = pal[2], linewidth = 0.8) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "19. geom_vline — Vertical Reference Lines",
            subtitle = "6-cylinder region between 2.6–3.5 displacement"))

  # 20. geom_abline
  fit <- lm(hwy ~ displ, data = mpg)
  pp(ggplot(mpg, aes(displ, hwy)) +
       geom_point(color = pal[1], alpha = 0.5) +
       geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],
                   color = pal[2], linewidth = 1) +
       theme_fn() +
       labs(title = "20. geom_abline — Linear Model Fit",
            subtitle = paste0("Slope = ", round(coef(fit)[2], 2),
                              ", Intercept = ", round(coef(fit)[1], 2))))

  # 21. geom_segment
  pp(ggplot(seg_df, aes(x = x, y = y, xend = xend, yend = yend, color = label)) +
       geom_segment(linewidth = 1.2,
                    arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "21. geom_segment — Directed Segments",
            subtitle = "Arrows from (x,y) to (xend,yend)"))

  # 22. geom_curve
  pp(ggplot(curve_df, aes(x = x, y = y, xend = xend, yend = yend)) +
       geom_curve(curvature = 0.3, color = pal[1], linewidth = 1,
                  arrow = arrow(length = unit(0.3, "cm"))) +
       geom_curve(curvature = -0.3, color = pal[2], linewidth = 1,
                  linetype = "dashed",
                  arrow = arrow(length = unit(0.3, "cm"))) +
       geom_point(aes(x = x, y = y), color = pal[3], size = 4) +
       geom_point(aes(x = xend, y = yend), color = pal[4], size = 4) +
       theme_fn() +
       labs(title = "22. geom_curve — Curved Connectors",
            subtitle = "Positive and negative curvature with arrows"))

  # 23. geom_spoke
  pp(ggplot(spoke_df, aes(x, y, angle = angle, radius = speed * 0.4)) +
       geom_spoke(color = pal[1], alpha = 0.6) +
       geom_point(aes(size = speed), color = pal[4], alpha = 0.5) +
       scale_size_continuous(range = c(0.5, 3)) +
       theme_fn() +
       labs(title = "23. geom_spoke — Directional Spokes",
            subtitle = "Lines at given angle and radius from each point"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 6: Rectangles & Shapes
  # ────────────────────────────────────────────────────────────────

  # 24. geom_rect
  pp(ggplot(rect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                         fill = group)) +
       geom_rect(alpha = 0.6, color = fg) +
       scale_fill_nepes(theme = theme_name) + theme_fn() +
       labs(title = "24. geom_rect — Rectangles",
            subtitle = "Defined by xmin/xmax/ymin/ymax"))

  # 25. geom_tile
  pp(ggplot(tile_df, aes(x, y, fill = z)) +
       geom_tile(color = bg, linewidth = 0.5) +
       scale_fill_gradient(low = pal[1], high = pal[2]) +
       theme_fn() +
       labs(title = "25. geom_tile — Tile Heatmap",
            subtitle = "5x5 grid with random values"))

  # 26. geom_polygon
  pp(ggplot(line_df, aes(x, y)) +
       geom_polygon(fill = pal[1], alpha = 0.4, color = pal[1]) +
       geom_point(color = pal[4], size = 3) +
       theme_fn() +
       labs(title = "26. geom_polygon — Filled Polygon",
            subtitle = "Vertices connected in data order, enclosed area filled"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 7: Text & Labels
  # ────────────────────────────────────────────────────────────────

  # 27. geom_text
  pp(ggplot(label_df, aes(wt, mpg, label = model)) +
       geom_point(color = pal[1], size = 2) +
       geom_text(hjust = -0.1, vjust = 0.5, size = 3, color = fg) +
       theme_fn() +
       labs(title = "27. geom_text — Text Labels",
            subtitle = "Car model names at data points") +
       xlim(1, 6))

  # 28. geom_label
  pp(ggplot(label_df, aes(wt, mpg, label = model)) +
       geom_point(color = pal[1], size = 2) +
       geom_label(hjust = -0.05, size = 2.5, fill = pal[1], color = "white",
                  alpha = 0.8, label.padding = unit(0.15, "lines")) +
       theme_fn() +
       labs(title = "28. geom_label — Boxed Labels",
            subtitle = "Text inside padded, filled boxes") +
       xlim(1, 6))

  # ────────────────────────────────────────────────────────────────
  # SECTION 8: Smoothing & Fitting
  # ────────────────────────────────────────────────────────────────

  # 29. geom_smooth (loess)
  pp(ggplot(mpg, aes(displ, hwy)) +
       geom_point(color = pal[1], alpha = 0.4) +
       geom_smooth(color = pal[2], fill = pal[2], alpha = 0.2) +
       theme_fn() +
       labs(title = "29. geom_smooth — LOESS Smooth",
            subtitle = "Default smoother with standard error band"))

  # 30. geom_smooth (lm, grouped)
  pp(ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
       geom_point(alpha = 0.5) +
       geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "30. geom_smooth — Linear Model by Group",
            subtitle = "Separate lm fit per cylinder count"))

  # 31. geom_quantile
  pp(suppressWarnings(
    ggplot(mpg, aes(displ, hwy)) +
      geom_point(color = pal[1], alpha = 0.4) +
      geom_quantile(quantiles = c(0.25, 0.5, 0.75), color = pal[2],
                    linewidth = 0.8) +
      theme_fn() +
      labs(title = "31. geom_quantile — Quantile Regression",
           subtitle = "25th, 50th, 75th percentile lines")
  ))

  # ────────────────────────────────────────────────────────────────
  # SECTION 9: Error / Uncertainty
  # ────────────────────────────────────────────────────────────────

  # 32. geom_linerange
  pp(ggplot(err_df, aes(treatment, response, color = group)) +
       geom_linerange(aes(ymin = lower, ymax = upper),
                      position = position_dodge(width = 0.3), linewidth = 0.8) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "32. geom_linerange — Vertical Line Range",
            subtitle = "Lower to upper boundaries"))

  # 33. geom_pointrange
  pp(ggplot(err_df, aes(treatment, response, color = group)) +
       geom_pointrange(aes(ymin = lower, ymax = upper),
                       position = position_dodge(width = 0.3)) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "33. geom_pointrange — Point + Range",
            subtitle = "Point at response with range to bounds"))

  # 34. geom_crossbar
  pp(ggplot(err_df, aes(treatment, response, color = group)) +
       geom_crossbar(aes(ymin = lower, ymax = upper),
                     position = position_dodge(width = 0.4), width = 0.3) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "34. geom_crossbar — Cross Bar",
            subtitle = "Horizontal bar at value with box to bounds"))

  # 35. geom_errorbar
  pp(ggplot(err_df, aes(treatment, response, color = group)) +
       geom_errorbar(aes(ymin = lower, ymax = upper),
                     position = position_dodge(width = 0.3), width = 0.2) +
       geom_point(position = position_dodge(width = 0.3), size = 2) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "35. geom_errorbar — Error Bars",
            subtitle = "Capped vertical error bars with points"))

  # 36. geom_errorbarh
  pp(ggplot(err_df, aes(response, treatment, color = group)) +
       geom_errorbarh(aes(xmin = lower, xmax = upper),
                      position = position_dodge(width = 0.3), height = 0.2) +
       geom_point(position = position_dodge(width = 0.3), size = 2) +
       scale_color_nepes(theme = theme_name) + theme_fn() +
       labs(title = "36. geom_errorbarh — Horizontal Error Bars",
            subtitle = "Error bars along x-axis"))

  # 37. geom_ribbon
  pp(ggplot(huron_df, aes(year, level)) +
       geom_ribbon(aes(ymin = level - 1, ymax = level + 1),
                   fill = pal[1], alpha = 0.3) +
       geom_line(color = pal[1], linewidth = 0.8) +
       theme_fn() +
       labs(title = "37. geom_ribbon — Confidence Band",
            subtitle = "Lake Huron water level ± 1 ft"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 10: 2D Density & Binning
  # ────────────────────────────────────────────────────────────────

  # 38. geom_density_2d (alias: geom_density2d)
  pp(ggplot(faithful, aes(eruptions, waiting)) +
       geom_point(color = pal[1], alpha = 0.3, size = 1) +
       geom_density_2d(color = pal[2]) +
       theme_fn() +
       labs(title = "38. geom_density_2d — 2D Density Contours",
            subtitle = "Old Faithful eruptions vs waiting time"))

  # 39. geom_density_2d_filled (alias: geom_density2d_filled)
  pp(ggplot(faithful, aes(eruptions, waiting)) +
       geom_density_2d_filled(alpha = 0.7) +
       theme_fn() +
       labs(title = "39. geom_density_2d_filled — Filled 2D Density",
            subtitle = "Color encodes density level"))

  # 40. geom_contour
  pp(ggplot(contour_df, aes(x, y, z = z)) +
       geom_contour(color = pal[1], bins = 8) +
       theme_fn() +
       labs(title = "40. geom_contour — Contour Lines",
            subtitle = "Pre-computed bivariate normal density"))

  # 41. geom_contour_filled
  pp(ggplot(contour_df, aes(x, y, z = z)) +
       geom_contour_filled(bins = 8) +
       theme_fn() +
       labs(title = "41. geom_contour_filled — Filled Contours",
            subtitle = "Filled regions between contour levels"))

  # 42. geom_bin_2d (alias: geom_bin2d)
  pp(ggplot(diamonds, aes(carat, price)) +
       geom_bin_2d(bins = 50) +
       scale_fill_gradient(low = pal[1], high = pal[2]) +
       theme_fn() +
       labs(title = "42. geom_bin_2d — 2D Rectangular Bins",
            subtitle = "Diamond carat vs price (53K observations)"))

  # 43. geom_hex
  if (requireNamespace("hexbin", quietly = TRUE)) {
    pp(ggplot(diamonds, aes(carat, price)) +
         geom_hex(bins = 40) +
         scale_fill_gradient(low = pal[1], high = pal[2]) +
         theme_fn() +
         labs(title = "43. geom_hex — Hexagonal Bins",
              subtitle = "Beehive pattern binning"))
  } else {
    message("Skipping geom_hex: install hexbin package")
  }

  # 44. geom_raster
  pp(ggplot(raster_df, aes(x, y, fill = z)) +
       geom_raster(interpolate = TRUE) +
       scale_fill_gradient(low = bg, high = pal[1]) +
       theme_fn() +
       labs(title = "44. geom_raster — Smooth Raster",
            subtitle = "Bivariate normal with interpolate = TRUE"))

  # ────────────────────────────────────────────────────────────────
  # SECTION 11: Maps & Spatial
  # ────────────────────────────────────────────────────────────────

  # 45. geom_map
  pp(ggplot(us_data, aes(map_id = region)) +
       geom_map(aes(fill = value), map = us_map) +
       expand_limits(x = us_map$long, y = us_map$lat) +
       scale_fill_gradient(low = pal[1], high = pal[2]) +
       theme_fn() +
       theme(axis.text = element_blank(), axis.ticks = element_blank()) +
       labs(title = "45. geom_map — Choropleth Map",
            subtitle = "Random values per US state", x = NULL, y = NULL))

  # 46. geom_polygon (US map)
  pp(ggplot(us_map, aes(long, lat, group = group)) +
       geom_polygon(fill = pal[1], color = bg, linewidth = 0.2) +
       coord_fixed(1.3) +
       theme_fn() +
       theme(axis.text = element_blank(), axis.ticks = element_blank()) +
       labs(title = "46. geom_polygon — US Map",
            subtitle = "States as filled polygons", x = NULL, y = NULL))

  # 47. geom_sf (if sf available)
  if (requireNamespace("sf", quietly = TRUE)) {
    nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
    pp(ggplot(nc) +
         geom_sf(aes(fill = AREA), color = bg, linewidth = 0.2) +
         scale_fill_gradient(low = pal[1], high = pal[2]) +
         theme_fn() +
         theme(axis.text = element_blank(), axis.ticks = element_blank()) +
         labs(title = "47. geom_sf — Simple Features Map",
              subtitle = "North Carolina counties by area"))

    # 48. geom_sf_text
    nc_sub <- nc[1:5, ]
    pp(ggplot(nc_sub) +
         geom_sf(fill = pal[1], alpha = 0.3, color = fg) +
         geom_sf_text(aes(label = NAME), color = fg, size = 3) +
         theme_fn() +
         theme(axis.text = element_blank(), axis.ticks = element_blank()) +
         labs(title = "48. geom_sf_text — SF Map with Text",
              subtitle = "County names as plain text"))

    # 49. geom_sf_label
    pp(ggplot(nc_sub) +
         geom_sf(fill = pal[1], alpha = 0.3, color = fg) +
         geom_sf_label(aes(label = NAME), size = 2.5,
                       fill = pal[1], color = "white", alpha = 0.8) +
         theme_fn() +
         theme(axis.text = element_blank(), axis.ticks = element_blank()) +
         labs(title = "49. geom_sf_label — SF Map with Labels",
              subtitle = "County names in padded boxes"))
  } else {
    message("Skipping geom_sf/sf_text/sf_label: install sf package")
  }

  # ────────────────────────────────────────────────────────────────
  # SECTION 12: Statistical
  # ────────────────────────────────────────────────────────────────

  # 50. geom_qq + 51. geom_qq_line
  pp(ggplot(qq_df, aes(sample = y)) +
       geom_qq(color = pal[1], alpha = 0.5) +
       geom_qq_line(color = pal[2], linewidth = 0.8) +
       theme_fn() +
       labs(title = "50–51. geom_qq + geom_qq_line — QQ Plot",
            subtitle = "t(5) sample vs normal theoretical quantiles"))

  # 52. geom_function
  pp(ggplot(func_df, aes(x)) +
       geom_histogram(aes(y = after_stat(density)), bins = 25,
                      fill = pal[1], alpha = 0.4, color = fg,
                      linewidth = 0.2) +
       geom_function(fun = dnorm, color = pal[2], linewidth = 1) +
       theme_fn() +
       labs(title = "52. geom_function — Overlay Theoretical Distribution",
            subtitle = "Normal density over histogram of random draws"))

  # 53. geom_blank
  pp(ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
       geom_blank() +
       theme_fn() +
       labs(title = "53. geom_blank — Empty Plot (Axis Limits Only)",
            subtitle = "Data mapped but nothing drawn; useful for layout prep"))

  # ────────────────────────────────────────────────────────────────
  # BONUS: SPC Control Chart (combines hline, point, line)
  # ────────────────────────────────────────────────────────────────
  viol_idx <- which(spc_df$value > ucl | spc_df$value < lcl)
  pp(ggplot(spc_df, aes(x, value)) +
       geom_line(color = spc_col["data_points"], linewidth = 0.5) +
       geom_point(color = spc_col["data_points"], size = 1.5) +
       geom_hline(yintercept = cl, color = spc_col["center_line"], linewidth = 1) +
       geom_hline(yintercept = c(ucl, lcl), color = spc_col["control_limit"],
                  linetype = "dashed", linewidth = 0.7) +
       {if (length(viol_idx) > 0)
         geom_point(data = spc_df[viol_idx, ], color = spc_col["violation"],
                    size = 3, shape = 18)} +
       theme_fn() +
       labs(title = "Bonus: SPC X-bar Control Chart",
            subtitle = "CL, UCL/LCL, violations — nepes_spc() palette",
            x = "Subgroup", y = "Value"))

  dev.off()
  cat("  Generated geom53-", theme_name, ".pdf\n", sep = "")
}

generate_showcase("dark")
generate_showcase("light")
cat("Done! 53 geom charts × 2 themes generated.\n")
