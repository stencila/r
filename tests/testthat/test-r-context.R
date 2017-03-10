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

  it("has an a call method", {
    expect_equal(s$call(''), list(errors=NULL, output=NULL))

    # Takes arguments
    expect_equal(s$call('list(a_is=a,b_is=b)',list(a=pack(42),b=pack('foo')))$output, pack(list(a_is=42,b_is='foo')))

    # Scope is local...
    expect_equal(s$call('x <- 42')$output, NULL)
    expect_equal(s$call('x')$errors$`1`, "object 'x' not found")

    # Last value is returned as per usual
    expect_equal(s$call('foo <- "bar"\nfoo'), list(errors=NULL, output=pack('bar')))

    # return() function can also be used - outputs the first returned value
    expect_equal(s$call('return(2)\nreturn("not this")')$output$content, '2')

    # Works multiline
    func <- 'if(x==1){
      "x is 1"
    } else if(x==2){
      return("x is 2")
    } else {
      "x is ?"
    }'
    expect_equal(unpack(s$call(func,list(x=pack(1)))$output), "x is 1")
    expect_equal(unpack(s$call(func,list(x=pack(2)))$output), "x is 2")
    expect_equal(unpack(s$call(func,list(x=pack(3)))$output), "x is ?")

    # Reports errors as expected
    expect_equal(s$call('x')$errors$`1`, "object 'x' not found")
    expect_equal(s$call('\nx\n')$errors$`2`, "object 'x' not found")
    expect_equal(s$call('1\nx')$errors$`2`, "object 'x' not found")
    expect_equal(s$call('\n\nx')$errors$`3`, "object 'x' not found")
  })

  it("has an a depends method", {
    expect_equal(s$depends('x'), 'x')
    expect_equal(s$depends('x+y/z+foo()'), c('x','y','z','foo'))
    expect_equal(s$depends('x<-1\nx+y/z+foo()'), c('x','y','z','foo'))
  })
})
