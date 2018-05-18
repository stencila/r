context("RContext")

describe("RContext", {

  it("can be constructed", {
    s <- RContext$new()

    expect_equal(class(s)[1], "RContext")
  })

  it("has an compile method", {
    s <- RContext$new()

    expect_equal(s$compile(""), list(
      inputs = list(),
      output = NULL,
      messages = list()
    ))

    expect_equal(s$compile("x * 2", exprOnly = TRUE), list(
      inputs = list("x"),
      output = NULL,
      messages = list()
    ))

    expect_equal(s$compile("x <- 2", exprOnly = TRUE), list(
      inputs = list(),
      output = NULL,
      messages = list(list(
        line = 0,
        column = 0,
        type = "error",
        message = "Code is not a single, simple expression"
      ))
    ))

    # x assigned and then used
    expect_equal(s$compile("x <- 2\nx"), list(
      inputs = list(),
      output = "x",
      messages = list()
    ))

    # x used and then assigned (this should not be allowed)
    expect_equal(s$compile("x\nx <- 2"), list(
      inputs = list("x"),
      output = NULL,
      messages = list(list(
        line = 0,
        column = 0,
        type = "warning",
        message = "Ignoring attempt to use a cell input \"x\" as a cell output"
      ))
    ))

    # globals are not included as inputs
    expect_equal(s$compile("cos(2 * pi * r)"), list(
      inputs = list("r"),
      output = NULL,
      messages = list()
    ))
  })

  it("has an execute method", {
    s <- RContext$new()

    expect_equal(s$execute(""), list(
      value = NULL,
      messages = list()
    ))

    expect_equal(s$execute("x <- 42")$value, NULL)
    expect_equal(s$execute("x")$value$data, 42)

    expect_equal(s$execute("y <- 3.14\ny")$value$data, 3.14)

    expect_equal(s$execute("foo")$messages, list(list(
      line = 1,
      column = 0,
      type = "error",
      message = "object \"foo\" not found"
    )))

    r <- s$execute("x*2\nfoo\nx")
    expect_equal(r$messages, list(list(
      line = 2,
      column = 0,
      type = "error",
      message = "object \"foo\" not found"
    )))
    expect_equal(r$value$data, 42)

    r <- s$execute("plot(1,1)")
    expect_equal(r$value$type, "image")
    expect_equal(str_sub(r$value$src, 1, 10), "data:image")

    # Load ggplot2 so that diamonds is available
    s$execute("library(ggplot2)")

    r <- s$execute("ggplot(diamonds) + geom_point(aes(x=carat, y=price))")
    expect_equal(r$value$type, "image")
    expect_equal(str_sub(r$value$src, 1, 10), "data:image")

    # An error in the rendering of the ggplot (in this case missing aesthtics)
    # which wil thow in the packing of the ggplot value
    r <- s$execute("ggplot(diamonds) + geom_point()")
    expect_equal(r$messages, list(list(
      line = 0,
      column = 0,
      type = "error",
      message = "geom_point requires the following missing aesthetics: x, y"
    )))
    expect_equal(r$value, NULL)

    # Takes arguments
    expect_equal(s$execute("list(a_is=a,b_is=b)", list(
      a = s$pack(42),
      b = s$pack("foo")
    ))$value, s$pack(list(a_is = 42, b_is = "foo")))

    # Last value is returned as per usual
    expect_equal(s$execute("foo <- 'bar'\nfoo")$value$data, "bar")

    # Works multiline
    func <- "if(x==1){
      'x is 1'
    } else if(x==2){
      return('x is 2')
    } else {
      'x is ?'
    }"
    expect_equal(s$unpack(s$execute(func, list(x = s$pack(1)))$value), "x is 1")
    expect_equal(s$unpack(s$execute(func, list(x = s$pack(2)))$value), "x is 2")
    expect_equal(s$unpack(s$execute(func, list(x = s$pack(3)))$value), "x is ?")

    # Reports errors as expecte
    expect_equal(s$execute("baz")$messages[[1]]$line, 1)
    expect_equal(s$execute("\nbaz\n")$messages[[1]]$line, 2)
    expect_equal(s$execute("1\nbaz")$messages[[1]]$line, 2)
    expect_equal(s$execute("\n\nbaz")$messages[[1]]$line, 3)
  })
})
