test_that("nepes_pal returns 12 colors by default", {
  pal <- nepes_pal()
  expect_length(pal, 12)
})

test_that("nepes_color returns correct hex colors for the light theme", {
  expected <- c(
    blue = "#23438E",
    orange = "#C25609",
    green = "#017939",
    red = "#C4181F",
    teal = "#2D7A82",
    purple = "#873D8E"
  )

  actual <- vapply(names(expected), nepes_color, character(1))
  expect_identical(unname(actual), unname(expected))
})

test_that("nepes_color returns correct hex colors for the dark theme", {
  expected <- c(
    blue = "#5C8CFF",
    orange = "#FEA413",
    green = "#3DDC84",
    red = "#FF5C5C",
    teal = "#3A9BA5",
    purple = "#A274C3"
  )

  actual <- vapply(
    names(expected),
    function(name) nepes_color(name, theme = "dark"),
    character(1)
  )
  expect_identical(unname(actual), unname(expected))
})

test_that("nepes_color errors on invalid color names", {
  expect_error(nepes_color("invalid"), "`name` must be one of:")
})

test_that("nepes_pal returns valid hex colors", {
  pal <- nepes_pal()
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("nepes_pal returns a named vector with expected names", {
  pal <- nepes_pal()
  expected_names <- c(
    "blue", "orange", "green", "red", "teal", "purple",
    "blue2", "orange2", "green2", "red2", "teal2", "purple2"
  )

  expect_named(pal, expected_names)
  expect_identical(names(pal), expected_names)
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

test_that("theme_nepes_light returns a ggplot2 theme", {
  th <- theme_nepes_light()
  expect_s3_class(th, "theme")
})

test_that("theme_nepes_dark returns a ggplot2 theme", {
  th <- theme_nepes_dark()
  expect_s3_class(th, "theme")
})
