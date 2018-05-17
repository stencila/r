if (require("lintr", quietly = TRUE)) {
  test_that("there is no lint", {
    lintr::expect_lint_free()
  })
}
