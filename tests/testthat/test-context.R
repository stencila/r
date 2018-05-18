describe("Context.pack()", {
  c <- Context$new()
  check <- function (x, json) {
    p <- c$pack(x)
    j <- to_json(p)
    expect_equal(j, json)
  }

  # nolint start

  it("works for primitive types", {
    check(NULL, '{"type":"null","data":null}')
    check(NA, '{"type":"null","data":null}')

    check(TRUE, '{"type":"boolean","data":true}')
    check(FALSE, '{"type":"boolean","data":false}')

    check(as.integer(42), '{"type":"integer","data":42}')
    check(as.integer(1000000000), '{"type":"integer","data":1000000000}')

    check(3.14, '{"type":"number","data":3.14}')
    check(pi, '{"type":"number","data":3.14159265358979}')

    check(1e10, '{"type":"number","data":10000000000}')
    check(1e-10, '{"type":"number","data":1e-10}')
  })

  it("works for lists", {
    check(
      list(a = 1, b = 3.14, c = "foo", d = list(e = 1, f = 2)),
      '{"type":"object","data":{"a":1,"b":3.14,"c":"foo","d":{"e":1,"f":2}}}'
    )
  })

  it("works for vectors", {
    check(vector(), '{"type":"array","data":[]}')
    check(1:4, '{"type":"array","data":[1,2,3,4]}')
    check(seq(1.1, 2.1, 1), '{"type":"array","data":[1.1,2.1]}')
  })

  it("works for data frames and matrices", {
    check(data.frame(), '{"type":"table","data":{"type":"table","columns":0,"rows":0,"data":{}}}')
    check(data.frame(a = 1:3), '{"type":"table","data":{"type":"table","columns":1,"rows":3,"data":{"a":[1,2,3]}}}')
    check(data.frame(a = 1:3, b = c("x", "y", "z")), '{"type":"table","data":{"type":"table","columns":2,"rows":3,"data":{"a":[1,2,3],"b":["x","y","z"]}}}')

    check(matrix(data = 1:4, nrow = 2), '{"type":"table","data":{"type":"table","columns":2,"rows":2,"data":{"V1":[1,2],"V2":[3,4]}}}')
  })

  # nolint end

  it("works for recorded plots", {
    # For recodPlot to work..
    png(tempfile())
    dev.control("enable")

    plot(mpg~disp, mtcars)
    p <- c$pack(recordPlot())
    expect_equal(p$type, "image")
    expect_equal(str_sub(p$src, 1, 10), "data:image")
  })

  if (require("ggplot2", quietly = T)) {
    it("works for ggplots", {
      p <- c$pack(ggplot(mtcars) + geom_point(aes(x = disp, y = mpg)))
      expect_equal(p$type, "image")
      expect_equal(str_sub(p$src, 1, 10), "data:image")
    })
  }
})

describe("Context.unpack()", {
  c <- Context$new()

  # nolint start

  it("can take a list or a JSON stringing", {
    expect_null(c$unpack('{"type":"null", "data":null}'))
    expect_null(c$unpack(list(type = "null", data = NULL)))
  })

  it("errors if package is malformed", {
    expect_error(c$unpack(1), "should be a list")
    expect_error(c$unpack(list()), "should have field `type`")
  })

  it("works for primitive types", {
    expect_null(c$unpack('{"type":"null","data":null}'))

    expect_true(c$unpack('{"type":"boolean","data":true}'))
    expect_false(c$unpack('{"type":"boolean","data":false}'))

    expect_equal(c$unpack('{"type":"integer","data":42}'), 42)
    expect_equal(c$unpack('{"type":"integer","data":1000000000}'), as.integer(1000000000))

    expect_equal(c$unpack('{"type":"float","data":3.12}'), 3.12)
    expect_equal(c$unpack('{"type":"float","data":1e20}'), 1e20)
  })

  it("works for objects", {
    expect_equivalent(c$unpack('{"type":"object","data":{}}'), list())
    expect_equal(c$unpack('{"type":"object","data":{"a":1,"b":"foo","c":[1,2,3]}}'), list(a = 1, b = "foo", c = 1:3))
  })

  it("works for arrays", {
    expect_equal(c$unpack('{"type":"array","data":[]}'), vector())
    expect_equal(c$unpack('{"type":"array","data":[1,2,3,4,5]}'), 1:5)
  })

  # nolint end

  it("works for tabular data", {
    df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
    expect_equal(
      c$unpack(c$pack(df)),
      df
    )
  })
})
