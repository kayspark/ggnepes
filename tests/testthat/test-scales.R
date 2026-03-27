test_that("scale_color_nepes returns a ggplot2 scale", {
  s <- scale_color_nepes()
  expect_s3_class(s, "Scale")
})

test_that("scale_fill_nepes_highlight returns a ggplot2 scale", {
  s <- scale_fill_nepes_highlight()
  expect_s3_class(s, "Scale")
})

test_that("scale_fill_nepes works with n colors", {
  s <- scale_fill_nepes()
  expect_identical(s$palette(4), nepes_pal(n = 4))
})
