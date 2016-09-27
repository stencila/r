test_that("Context an be constructed", {
  c = Context$new()

  expect_equal(class(c)[1], "Context")
})
