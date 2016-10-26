test_that("Session can be constructed", {
  s <- RSession$new()

  expect_equal(class(s)[1], "RSession")
  expect_equal(s$type, "session-r")
})

test_that("Session execute", {
  s <- RSession$new()

  expect_equal(s$execute(''), list(errors=list(), output=list()))

  expect_equal(s$execute('x <- 42'), list(errors=list(), output=list()))
  expect_equal(s$execute('x'), list(errors=list(), output=list(format='text', content='42')))

  expect_equal(s$execute('y <- 3.14\ny'), list(errors=list(), output=list(format='text', content='3.14')))

  expect_equal(s$execute('foo'), list(errors=list("1"="object 'foo' not found"), output=list()))
  expect_equal(s$execute('x*2\nfoo\nx'), list(errors=list("2"="object 'foo' not found"), output=list(format='text', content='42')))
})

test_that("Session execute with base graphics returns a PNG", {
  s <- RSession$new()
  res <- s$execute('plot(1,1)')
  expect_equal(res$output$format, 'png')
  expect_equal(str_sub(res$output$content, 1, 21), 'data:image/png;base64')
})

test_that("Session execute with a ggplot returns a PNG", {
  if (require('ggplot2', quietly=T)) {
    s <- RSession$new()
    res <- s$execute('library(ggplot2); ggplot(diamonds) + geom_point(aes(x=carat, y=price))')
    expect_equal(res$output$format, 'png')
    expect_equal(str_sub(res$output$content, 1, 21), 'data:image/png;base64')
  }
})

test_that("Session can acesss self, parent, children and siblings within environment", {
  s1 <- RSession$new()
  s2 <- RSession$new()

  # No parent yet
  expect_equal(s2$print('self$id'), s2$id)
  expect_equal(s2$print('parent'), '')

  # Establish parent - child relation
  s2$parent <- s1
  expect_equal(s2$parent$id, s1$id)
  expect_equal(s2$print('parent$id'), s1$id)

  # Get a variable from parent
  s1$execute('x <- 42')
  expect_equal(s2$print('parent$get("x")'), '42')

  # Set a variable in parent
  s2$execute('parent$set("x", 84)')
  expect_equal(s1$print('x'), '84')
  expect_equal(s2$print('parent$get("x")'), '84')

})

test_that("Session can spawn other sessions", {
  s <- RSession$new()

  s$spawn('r')
  expect_equal(s$child('r')$type, 'session-r')
  expect_equal(s$child('r'), s$child('r'))

})


