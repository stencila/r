test_that("Document an be constructed", {
  c = Document$new()

  expect_equal(class(c)[1], "Document")
})
