test_that("Component an be constructed", {
  c = Component$new()

  expect_equal(class(c)[1], "Component")
})
