describe('type()', {
  expect_equal(type(NULL), 'null')

  expect_equal(type(TRUE), 'boolean')
  expect_equal(type(FALSE), 'boolean')

  expect_equal(type(as.integer(42)), 'integer')
  expect_equal(type(as.integer(1000000000)), 'integer')

  expect_equal(type(3.14), 'float')
  expect_equal(type(pi), 'float')
  expect_equal(type(1.1e-20), 'float')

  expect_equal(type(''), 'string')
  expect_equal(type('Yo!'), 'string')

  expect_equal(type(vector()), 'array')
  expect_equal(type(c(1, 2, 3)), 'array')

  expect_equal(type(list()), 'object')
  expect_equal(type(list(a=1, b=2)), 'object')

  expect_equal(type(data.frame(a=1:10, b=11:20)), 'table')

  expect_equal(type(list(type='html', content='<img>')), 'html')
  expect_equal(type(list(type='png', content='')), 'png')
  expect_equal(type(list(type=1)), 'object')

  expect_equal(type(function () {}), 'unknown')
})

describe('pack()', {
  check <- function (x, type, format, content) {
    p <- pack(x)
    expect_equal(p$type, type)
    expect_equal(p$format, format)
    expect_equal(p$content, content)
  }

  it("works for primitive types", {
    check(NULL, 'null', 'text', 'null')
    check(NA, 'null', 'text', 'null')

    check(TRUE, 'boolean', 'text', 'true')
    check(FALSE, 'boolean', 'text', 'false')

    check(as.integer(42), 'integer', 'text', '42')
    check(as.integer(1000000000), 'integer', 'text', '1000000000')

    check(3.14, 'float', 'text', '3.14')
    check(pi, 'float', 'text', '3.14159265358979')

    check(1e10, 'float', 'text', '1e+10')
    check(1e-10, 'float', 'text', '1e-10')
  })

  it("works for lists", {
    check(list(), 'object', 'json', '{}')
    check(list(a=1, b=3.14, c='foo', d=list(e=1, f=2)), 'object', 'json', '{"a":1,"b":3.14,"c":"foo","d":{"e":1,"f":2}}')
  })

  it("works for vectors", {
    check(vector(), 'array', 'json', '[]')
    check(1:4, 'array', 'json', '[1,2,3,4]')
    check(seq(1.1,2.1,1), 'array', 'json', '[1.1,2.1]')
  })

  it("works for data frames and matrices", {
    check(data.frame(), 'table', 'json', '{"type":"table","data":{}}')
    check(data.frame(a=1:3), 'table', 'json', '{"type":"table","data":{"a":{"values":[1,2,3],"type":"quantitative"}}}')
    check(data.frame(a=1:3,b=c('x','y','z')), 'table', 'json', '{"type":"table","data":{"a":{"values":[1,2,3],"type":"quantitative"},"b":{"values":["x","y","z"],"type":"nominal"}}}')

    check(matrix(), 'null', 'text', 'null')
    check(matrix(data=1:4,nrow=2), 'table', 'json', '{"type":"table","data":{"V1":{"values":[1,2],"type":"quantitative"},"V2":{"values":[3,4],"type":"quantitative"}}}')
  })

  it("works for recorded plots", {
    # For recodPlot to work..
    png(tempfile())
    dev.control('enable')

    plot(mpg~disp, mtcars)
    p <- pack(recordPlot())
    expect_equal(p$type, 'image')
    expect_equal(p$format, 'png')
    expect_equal(str_sub(p$content, 1, 10), 'iVBORw0KGg')
  })

  if (require('ggplot2', quietly=T)) {
    it("works for ggplots", {
      p <- pack(ggplot(mtcars) + geom_point(aes(x=disp,y=mpg)))
      expect_equal(p$type, 'image')
      expect_equal(p$format, 'png')
      expect_equal(str_sub(p$content, 1, 10), 'iVBORw0KGg')
    })
  }
})

describe('unpack()', {
  it("can take a list or a JSON stringing", {
    expect_null(unpack('{"type":"null","format":"text","content":"null"}'))
    expect_null(unpack(list(type='null',format='text',content='null')))
  })

  it("errors if package is malformed", {
    expect_error(unpack(1), 'should be a list')

    expect_error(unpack(list()), 'should have fields `type`, `format`, `content`')
    expect_error(unpack("{}"))
    expect_error(unpack(list(type='null')))
    expect_error(unpack(list(type='null', format='text')))
  })

  it("works for primitive types", {
    expect_null(unpack(list(type='null',format='text',content='null')))

    expect_true(unpack(list(type='boolean',format='text',content='true')))
    expect_false(unpack(list(type='boolean',format='text',content='false')))

    expect_equal(unpack(list(type='integer',format='text',content='42')), 42)
    expect_equal(unpack(list(type='integer',format='text',content='1000000000')), as.integer(1000000000))

    expect_equal(unpack(list(type='float',format='text',content='3.12')), 3.12)
    expect_equal(unpack(list(type='float',format='text',content='1e20')), 1e20)
  })

  it("works for objects", {
    expect_equivalent(unpack(list(type='object',format='json',content='{}')), list())
    expect_equal(unpack(list(type='object',format='json',content='{"a":1,"b":"foo","c":[1,2,3]}')), list(a=1,b="foo",c=1:3))
  })

  it("works for arrayays", {
    expect_equal(unpack(list(type='array',format='json',content='[]')), vector())
    expect_equal(unpack(list(type='array',format='json',content='[1,2,3,4,5]')), 1:5)
  })

  it("works for tabular data", {
    expect_equal(
      unpack(list(type='table',format='csv',content='a,b\n1,x\n2,y\n3,z')),
      data.frame(a=1:3, b=c('x','y','z'), stringsAsFactors=FALSE)
    )

    expect_equal(
      unpack(list(type='table',format='json',content='{"type":"table","data":{"a":{"values":[1,2,3]}, "b":{"values":["x","y","z"]}}}')),
      data.frame(a=1:3, b=c('x','y','z'), stringsAsFactors=FALSE)
    )
  })
})




