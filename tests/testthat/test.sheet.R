test_that("Sheet an be constructed", {
  c = Sheet$new()

  expect_equal(class(c)[1], "Sheet")
})
