describe('RContext', {

  it("can be constructed", {
    s <- RContext$new()

    expect_equal(class(s)[1], "RContext")
  })

  it("has an runCode method", {
    s <- RContext$new()

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
    expect_equal(r$output$type, 'image')
    expect_equal(r$output$format, 'src')
    expect_equal(str_sub(r$output$content, 1, 10), 'data:image')

    # Load ggplot2 so that diamonds is available
    s$runCode('library(ggplot2)')

    r <- s$runCode('ggplot(diamonds) + geom_point(aes(x=carat, y=price))')
    expect_equal(r$output$type, 'image')
    expect_equal(r$output$format, 'src')
    expect_equal(str_sub(r$output$content, 1, 10), 'data:image')

    # An error in the rendering of the ggplot (in this case missing aesthtics)
    # which wil thow in the packing of the ggplot value
    r <- s$runCode('ggplot(diamonds) + geom_point()')
    expect_equal(r$errors, list(list(
      line = 0,
      column = 0,
      message = 'geom_point requires the following missing aesthetics: x, y'
    )))
    expect_equal(r$output, NULL)

  })

  it("has an a callCode method", {
    s <- RContext$new()

    expect_equal(s$callCode(''), list(errors=NULL, output=NULL))

    # Takes arguments
    expect_equal(s$callCode('list(a_is=a,b_is=b)',list(a=pack(42),b=pack('foo')))$output, pack(list(a_is=42,b_is='foo')))

    # Scope is local...
    expect_equal(s$callCode('x <- 42')$output, NULL)
    expect_equal(s$callCode('x')$errors[[1]]$message, "object 'x' not found")

    # Last value is returned as per usual
    expect_equal(s$callCode('foo <- "bar"\nfoo'), list(errors=NULL, output=pack('bar')))

    # return() function can also be used - outputs the first returned value
    result <- s$callCode('return(2)\nreturn("not this")')
    expect_equal(result$errors, NULL)
    expect_equal(result$output$content, '2')

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

    # Can call code isolated from context's global env, or not
    s$runCode('var1 <- 123')
    expect_equal(unpack(s$callCode('var1')$output), 123)
    result <- s$callCode('var1', isolated=TRUE)
    expect_equal(result$output, NULL)
    expect_equal(result$errors[[1]]$message, "object 'var1' not found")
  })

  it("has an a codeDependencies method", {
    s <- RContext$new()

    expect_equal(s$codeDependencies('x'), 'x')
    expect_equal(s$codeDependencies('x+y/z+foo()'), c('x','y','z','foo'))
    expect_equal(s$codeDependencies('x<-1\nx+y/z+foo()'), c('x','y','z','foo'))
  })
})
