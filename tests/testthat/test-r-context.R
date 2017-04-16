describe('RContext', {
  s <- RContext$new()

  it("can be constructed", {
    expect_equal(class(s)[1], "RContext")
  })

  it("has an runCode method", {
    expect_equal(s$runCode(''), list(errors=NULL, output=NULL))

    expect_equal(s$runCode('x <- 42')$output, NULL)
    expect_equal(s$runCode('x')$output, pack(42))

    expect_equal(s$runCode('y <- 3.14\ny')$output, pack(3.14))

    expect_equal(s$runCode('foo')$errors, list(list(
      line = 1,
      column = 0,
      message = "object 'foo' not found"
    )))

    r <- s$runCode('x*2\nfoo\nx')
    expect_equal(r$errors, list(list(
      line = 2,
      column = 0,
      message = "object 'foo' not found"
    )))
    expect_equal(r$output, pack(42))

    r <- s$runCode('plot(1,1)')
    expect_equal(r$output$type, 'plot')
    expect_equal(r$output$format, 'png')
    expect_equal(str_sub(r$output$content, 1, 10), 'iVBORw0KGg')

    if (require('ggplot2', quietly=T)) {
      r <- s$runCode('library(ggplot2); ggplot(diamonds) + geom_point(aes(x=carat, y=price))')
      expect_equal(r$output$type, 'plot')
      expect_equal(r$output$format, 'png')
      expect_equal(str_sub(r$output$content, 1, 10), 'iVBORw0KGg')
    }
  })

  it("has an a callCode method", {
    expect_equal(s$callCode(''), list(errors=NULL, output=NULL))

    # Takes arguments
    expect_equal(s$callCode('list(a_is=a,b_is=b)',list(a=pack(42),b=pack('foo')))$output, pack(list(a_is=42,b_is='foo')))

    # Scope is local...
    expect_equal(s$callCode('x <- 42')$output, NULL)
    expect_equal(s$callCode('x')$errors[[1]]$message, "object 'x' not found")

    # Last value is returned as per usual
    expect_equal(s$callCode('foo <- "bar"\nfoo'), list(errors=NULL, output=pack('bar')))

    # return() function can also be used - outputs the first returned value
    expect_equal(s$callCode('return(2)\nreturn("not this")')$output$content, '2')

    # Works multiline
    func <- 'if(x==1){
      "x is 1"
    } else if(x==2){
      return("x is 2")
    } else {
      "x is ?"
    }'
    expect_equal(unpack(s$callCode(func,list(x=pack(1)))$output), "x is 1")
    expect_equal(unpack(s$callCode(func,list(x=pack(2)))$output), "x is 2")
    expect_equal(unpack(s$callCode(func,list(x=pack(3)))$output), "x is ?")

    # Reports errors as expected
    expect_equal(s$callCode('x')$errors[[1]]$line, 1)
    expect_equal(s$callCode('\nx\n')$errors[[1]]$line, 2)
    expect_equal(s$callCode('1\nx')$errors[[1]]$line, 2)
    expect_equal(s$callCode('\n\nx')$errors[[1]]$line, 3)
  })

  it("has an a codeDependencies method", {
    expect_equal(s$codeDependencies('x'), 'x')
    expect_equal(s$codeDependencies('x+y/z+foo()'), c('x','y','z','foo'))
    expect_equal(s$codeDependencies('x<-1\nx+y/z+foo()'), c('x','y','z','foo'))
  })
})
