test_that("RSession can be constructed", {
  s <- RSession$new()

  expect_equal(class(s)[1], "RSession")
  expect_equal(s$type, "r-session")
})

test_that("RSession execute", {
  s <- RSession$new()

  #expect_equal(s$execute(''), list(errors=list(), output=list(), pipes=list()))

  expect_equal(s$execute('x <- 42')$output, NULL)
  expect_equal(s$execute('x')$output, pack(42))

  expect_equal(s$execute('y <- 3.14\ny')$output, pack(3.14))

  expect_equal(s$execute('foo')$errors, list("1"="object 'foo' not found"))

  r <- s$execute('x*2\nfoo\nx')
  expect_equal(r$errors, list("2"="object 'foo' not found"))
  expect_equal(r$output, pack(42))
})

test_that("RSession execute with base graphics returns a PNG", {
  s <- RSession$new()
  res <- s$execute('plot(1,1)')
  expect_equal(res$output$format, 'png')
  expect_equal(str_sub(res$output$value, 1, 21), 'data:image/png;base64')
})

test_that("RSession execute with a ggplot returns a PNG", {
  if (require('ggplot2', quietly=T)) {
    s <- RSession$new()
    res <- s$execute('library(ggplot2); ggplot(diamonds) + geom_point(aes(x=carat, y=price))')
    expect_equal(res$output$format, 'png')
    expect_equal(str_sub(res$output$value, 1, 21), 'data:image/png;base64')
  }
})

