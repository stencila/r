context("RContext")

describe("RContext", {

  it("can be constructed", {
    s <- RContext$new()

    expect_equal(class(s)[1], "RContext")
  })

  it("has a compile method", {
    context <- RContext$new()

    check <- function (source, expected) {
      cell <- context$compile(source)
      expect_equal(cell$type, "cell")
      expect_equal(cell[c("inputs", "outputs", "messages")], expected)
    }

    check("", list(
      inputs = list(),
      outputs = list(),
      messages = list()
    ))

    # x assigned and then used
    check("x <- 2\nx", list(
      inputs = list(),
      outputs = list(list(name = "x")),
      messages = list()
    ))

    # x used and then assigned (this should not be allowed)
    check("x\nx <- 2", list(
      inputs = list(list(name = "x")),
      outputs = list(),
      messages = list(list(
        line = 0,
        column = 0,
        type = "warning",
        message = "Ignoring attempt to use a cell input \"x\" as a cell output"
      ))
    ))

    # globals are not included as inputs
    check("cos(2 * pi * r)", list(
      inputs = list(list(name = "r")),
      outputs = list(),
      messages = list()
    ))

    # Expression cells
    cell <- context$compile(list(
      source = context$pack("x * 2"),
      expr = TRUE
    ))
    expect_equal(cell[c("inputs", "outputs", "messages")], list(
      inputs = list(list(name = "x")),
      outputs = list(),
      messages = list()
    ))

    cell <- context$compile(list(
      source = context$pack("x <- 2"),
      expr = TRUE
    ))
    expect_equal(cell[c("inputs", "outputs", "messages")], list(
      inputs = list(),
      outputs = list(),
      messages = list(list(
        line = 0,
        column = 0,
        type = "error",
        message = "Code is not a single, simple expression"
      ))
    ))
  })

  it("has an execute method", {
    skip("in progress")

    context <- RContext$new()

    expect_equal(context$execute(""), list(
      value = NULL,
      messages = list()
    ))

    expect_equal(context$execute("x <- 42")$value, NULL)
    expect_equal(context$execute("x")$value$data, 42)

    expect_equal(context$execute("y <- 3.14\ny")$value$data, 3.14)

    expect_equal(context$execute("foo")$messages, list(list(
      line = 1,
      column = 0,
      type = "error",
      message = "object \"foo\" not found"
    )))

    r <- context$execute("x*2\nfoo\nx")
    expect_equal(r$messages, list(list(
      line = 2,
      column = 0,
      type = "error",
      message = "object \"foo\" not found"
    )))
    expect_equal(r$value$data, 42)

    r <- context$execute("plot(1,1)")
    expect_equal(r$value$type, "image")
    expect_equal(str_sub(r$value$src, 1, 10), "data:image")

    # Load ggplot2 so that diamonds is available
    context$execute("library(ggplot2)")

    r <- context$execute("ggplot(diamonds) + geom_point(aes(x=carat, y=price))")
    expect_equal(r$value$type, "image")
    expect_equal(str_sub(r$value$src, 1, 10), "data:image")

    # An error in the rendering of the ggplot (in this case missing aesthtics)
    # which wil thow in the packing of the ggplot value
    r <- context$execute("ggplot(diamonds) + geom_point()")
    expect_equal(r$messages, list(list(
      line = 0,
      column = 0,
      type = "error",
      message = "geom_point requires the following missing aesthetics: x, y"
    )))
    expect_equal(r$value, NULL)

    # Takes arguments
    expect_equal(context$execute("list(a_is=a,b_is=b)", list(
      a = context$pack(42),
      b = context$pack("foo")
    ))$value, context$pack(list(a_is = 42, b_is = "foo")))

    # Last value is returned as per usual
    expect_equal(context$execute("foo <- 'bar'\nfoo")$value$data, "bar")

    # Works multiline
    func <- "if(x==1){
      'x is 1'
    } else if(x==2){
      return('x is 2')
    } else {
      'x is ?'
    }"
    expect_equal(context$unpack(context$execute(func, list(x = context$pack(1)))$value), "x is 1")
    expect_equal(context$unpack(context$execute(func, list(x = context$pack(2)))$value), "x is 2")
    expect_equal(context$unpack(context$execute(func, list(x = context$pack(3)))$value), "x is ?")

    # Reports errors as expecte
    expect_equal(context$execute("baz")$messages[[1]]$line, 1)
    expect_equal(context$execute("\nbaz\n")$messages[[1]]$line, 2)
    expect_equal(context$execute("1\nbaz")$messages[[1]]$line, 2)
    expect_equal(context$execute("\n\nbaz")$messages[[1]]$line, 3)
  })
})
