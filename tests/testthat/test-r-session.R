describe("RSession", {
  s <- RSession$new()

  it("can be constructed", {
    expect_equal(class(s)[1], "RSession")
    expect_equal(s$type, "r-session")
  })


  it("has an execute method which can be called without inputs", {
    expect_equal(s$execute(''), list(errors=NULL, output=NULL))

    expect_equal(s$execute('x <- 42')$output, NULL)
    expect_equal(s$execute('x')$output, pack(42))

    expect_equal(s$execute('y <- 3.14\ny')$output, pack(3.14))

    expect_equal(s$execute('foo')$errors, list("1"="object 'foo' not found"))

    r <- s$execute('x*2\nfoo\nx')
    expect_equal(r$errors, list("2"="object 'foo' not found"))
    expect_equal(r$output, pack(42))

    r <- s$execute('plot(1,1)')
    expect_equal(r$output$format, 'png')
    expect_equal(str_sub(r$output$value, 1, 21), 'data:image/png;base64')

    if (require('ggplot2', quietly=T)) {
      r <- s$execute('library(ggplot2); ggplot(diamonds) + geom_point(aes(x=carat, y=price))')
      expect_equal(r$output$format, 'png')
      expect_equal(str_sub(r$output$value, 1, 21), 'data:image/png;base64')
    }
  })

  it("has an execute method which can be called with inputs", {
    expect_equal(s$execute('a*b',list(a=pack(2),b=pack(3)))$output, pack(6))
  })

})
