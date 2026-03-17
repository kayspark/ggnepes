# ggnepes

Nepes corporate color palette for ggplot2.

Provides 12-color discrete palettes (light and dark), SPC semantic palettes,
matching ggplot2 scales, and complete themes derived from the
[nepes-palette](https://github.com/kayspark/nepes-palette) colorscheme.

## Installation

```r
# install.packages("devtools")
devtools::install_github("kayspark/ggnepes")
```

## Usage

```r
library(ggplot2)
library(ggnepes)

# Discrete color scale
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_nepes() +
  theme_nepes_light()

# Dark theme
ggplot(mpg, aes(class, fill = class)) +
  geom_bar() +
  scale_fill_nepes("dark") +
  theme_nepes_dark()

# Get raw palette vectors
nepes_pal()            # 12 light colors
nepes_pal("dark", 6)   # first 6 dark colors

# SPC palette
nepes_spc()            # named vector: center_line, data_points, ...
nepes_spc("dark")
```

## Palettes

| Theme | Colors |
|-------|--------|
| Light | 6 bases + 6 midtones (WCAG AA on white) |
| Dark  | 6 bases + 6 midtones (WCAG AA on #1E1C1A) |

SPC palettes provide semantic colors for Statistical Process Control charts:
center line, data points, control limits, spec limits, and violations.

## License

MIT - Kay Park / Nepes Co., Ltd.
