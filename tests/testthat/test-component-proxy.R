test_that("ComponentProxy can be defined with URL and type", {
  return()

  # This needs to be run with an exernal host because
  # otherwise get a timeout making request to self
  c <- Component$new()
  p <- ComponentProxy(c$type, c$url)

  expect_equal(class(p)[1], "ComponentProxy")
  expect_equal(c$type, p$type)

  expect_equal(c$address, p$address)
})

