if (require("lintr", quietly = TRUE)) {
  context("Linting")

  test_that("there is no lint", {
    lintr::expect_lint_free()
  })
}
