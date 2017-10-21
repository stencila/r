describe('RContext', {

  it("can be constructed", {
    s <- RContext$new()

    expect_equal(class(s)[1], "RContext")
  })

  it("has an analyseCode method", {
    s <- RContext$new()

    expect_equal(s$analyseCode(''), list(
      inputs=character(),
      output=NULL,
      messages=NULL
    ))

    expect_equal(s$analyseCode('x * 2', exprOnly=TRUE), list(
      inputs='x',
      output=NULL,
      messages=NULL
    ))

    expect_equal(s$analyseCode('x <- 2', exprOnly=TRUE), list(
      inputs=character(),
      output=NULL,
      messages=list(
        line=0,
        column=0,
        type='error',
        message='Code is not a single, simple expression'
      )
    ))

    # x assigned and then used
    expect_equal(s$analyseCode('x <- 2\nx'), list(
      inputs=character(),
      output='x',
      messages=NULL
    ))

    # x used and then assigned (this should not be allowed)
    expect_equal(s$analyseCode('x\nx <- 2'), list(
      inputs='x',
      output='x',
      messages=NULL
    ))

    # globals are not included as inputs
    expect_equal(s$analyseCode('cos(2 * pi * r)'), list(
      inputs='r',
      output=NULL,
      messages=NULL
    ))
  })

  it("has an executeCode method", {
    s <- RContext$new()

    expect_equal(s$executeCode(''), list(
      value=NULL,
      messages=NULL
    ))

    expect_equal(s$executeCode('x <- 42')$value, NULL)
    expect_equal(s$executeCode('x')$value$data, 42)

    expect_equal(s$executeCode('y <- 3.14\ny')$value$data, 3.14)

    expect_equal(s$executeCode('foo')$messages, list(list(
      line = 1,
      column = 0,
      type = 'error',
      message = "object 'foo' not found"
    )))

    r <- s$executeCode('x*2\nfoo\nx')
    expect_equal(r$messages, list(list(
      line = 2,
      column = 0,
      type = 'error',
      message = "object 'foo' not found"
    )))
    expect_equal(r$value$data, 42)

    r <- s$executeCode('plot(1,1)')
    expect_equal(r$value$type, 'image')
    expect_equal(str_sub(r$value$src, 1, 10), 'data:image')

    # Load ggplot2 so that diamonds is available
    s$executeCode('library(ggplot2)')

    r <- s$executeCode('ggplot(diamonds) + geom_point(aes(x=carat, y=price))')
    expect_equal(r$value$type, 'image')
    expect_equal(str_sub(r$value$src, 1, 10), 'data:image')

    # An error in the rendering of the ggplot (in this case missing aesthtics)
    # which wil thow in the packing of the ggplot value
    r <- s$executeCode('ggplot(diamonds) + geom_point()')
    expect_equal(r$messages, list(list(
      line = 0,
      column = 0,
      type = 'error',
      message = 'geom_point requires the following missing aesthetics: x, y'
    )))
    expect_equal(r$value, NULL)

    # Takes arguments
    expect_equal(s$executeCode('list(a_is=a,b_is=b)',list(
      a=s$pack(42),
      b=s$pack('foo')
    ))$value, s$pack(list(a_is=42,b_is='foo')))

    # Last value is returned as per usual
    expect_equal(s$executeCode('foo <- "bar"\nfoo')$value$data, 'bar')

    # Works multiline
    func <- 'if(x==1){
      "x is 1"
    } else if(x==2){
      return("x is 2")
    } else {
      "x is ?"
    }'
    expect_equal(s$unpack(s$executeCode(func,list(x=s$pack(1)))$value), "x is 1")
    expect_equal(s$unpack(s$executeCode(func,list(x=s$pack(2)))$value), "x is 2")
    expect_equal(s$unpack(s$executeCode(func,list(x=s$pack(3)))$value), "x is ?")

    # Reports errors as expecte
    expect_equal(s$executeCode('baz')$messages[[1]]$line, 1)
    expect_equal(s$executeCode('\nbaz\n')$messages[[1]]$line, 2)
    expect_equal(s$executeCode('1\nbaz')$messages[[1]]$line, 2)
    expect_equal(s$executeCode('\n\nbaz')$messages[[1]]$line, 3)
  })


  it("has a getFunction method", {
    s <- RContext$new()

    xml <- s$getFunction('sum')
    doc <- xml2::read_xml(xml)
    expect_equal(xml2::xml_text(xml2::xml_find_first(doc, '/function/name')), "sum")
  })
})
