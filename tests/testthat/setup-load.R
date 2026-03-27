if (dir.exists("R")) {
  r_dir <- "R"
} else if (dir.exists(file.path("..", "..", "R"))) {
  r_dir <- file.path("..", "..", "R")
} else {
  stop("Could not locate the package R directory for tests.", call. = FALSE)
}

for (path in list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)) {
  source(path, local = globalenv())
}
