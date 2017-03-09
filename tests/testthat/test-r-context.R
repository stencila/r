describe('RContext', {
  s <- RContext$new()

  it("can be constructed", {
    expect_equal(class(s)[1], "RContext")
  })


  it("has an run method", {
    expect_equal(s$run(''), list(errors=NULL, output=NULL))

    expect_equal(s$run('x <- 42')$output, NULL)
    expect_equal(s$run('x')$output, pack(42))

    expect_equal(s$run('y <- 3.14\ny')$output, pack(3.14))

    expect_equal(s$run('foo')$errors, list("1"="object 'foo' not found"))

    r <- s$run('x*2\nfoo\nx')
    expect_equal(r$errors, list("2"="object 'foo' not found"))
    expect_equal(r$output, pack(42))

    r <- s$run('plot(1,1)')
    expect_equal(r$output$type, 'plot')
    expect_equal(r$output$format, 'png')
    expect_equal(str_sub(r$output$content, 1, 21), 'data:image/png;base64')

    if (require('ggplot2', quietly=T)) {
      r <- s$run('library(ggplot2); ggplot(diamonds) + geom_point(aes(x=carat, y=price))')
      expect_equal(r$output$type, 'plot')
      expect_equal(r$output$format, 'png')
      expect_equal(str_sub(r$output$content, 1, 21), 'data:image/png;base64')
    }
  })

})
