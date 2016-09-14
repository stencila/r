test_that("Session an be constructed", {
  c = Session$new()

  expect_equal(class(c)[1], "Session")
})
