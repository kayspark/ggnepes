test_that("nepes_pal returns 12 colors by default", {
  pal <- nepes_pal()
  expect_length(pal, 12)
})

test_that("nepes_pal returns valid hex colors", {
  pal <- nepes_pal()
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("nepes_pal n parameter works", {
  expect_length(nepes_pal(n = 6), 6)
  expect_length(nepes_pal(n = 3), 3)
})

test_that("nepes_pal dark theme works", {
  pal <- nepes_pal("dark")
  expect_length(pal, 12)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("nepes_spc returns named vector", {
  spc <- nepes_spc()
  expect_length(spc, 5)
  expect_named(spc, c("center_line", "data_points", "control_limit",
                       "spec_limit", "violation"))
})

test_that("scale_color_nepes returns a ggplot2 scale", {
  s <- scale_color_nepes()
  expect_s3_class(s, "Scale")
})

test_that("theme_nepes_light returns a ggplot2 theme", {
  th <- theme_nepes_light()
  expect_s3_class(th, "theme")
})

test_that("theme_nepes_dark returns a ggplot2 theme", {
  th <- theme_nepes_dark()
  expect_s3_class(th, "theme")
})
