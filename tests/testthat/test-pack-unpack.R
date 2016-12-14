check <- function (x, type, format, value) {
  p <- pack(x)
  expect_equal(p$type, type)
  expect_equal(p$format, format)
  expect_equal(p$value, value)
}

test_that("the pack function works for primitive types", {
  check(NULL, 'null', 'text', 'null')
  check(NA, 'null', 'text', 'null')

  check(TRUE, 'bool', 'text', 'true')
  check(FALSE, 'bool', 'text', 'false')

  check(as.integer(42), 'int', 'text', '42')
  check(as.integer(1000000000), 'int', 'text', '1000000000')

  check(3.14, 'flt', 'text', '3.14')
  check(pi, 'flt', 'text', '3.14159265358979')

  check(1e10, 'flt', 'text', '1e+10')
  check(1e-10, 'flt', 'text', '1e-10')
})

test_that("the pack function works for lists", {
  check(list(), 'obj', 'json', '{}')
  check(list(a=1, b=3.14, c='foo', d=list(e=1, f=2)), 'obj', 'json', '{"a":1,"b":3.14,"c":"foo","d":{"e":1,"f":2}}')
})

test_that("the pack function works for vectors", {
  check(vector(), 'arr', 'json', '[]')
  check(1:4, 'arr', 'json', '[1,2,3,4]')
  check(seq(1.1,2.1,1), 'arr', 'json', '[1.1,2.1]')
})

test_that("the pack function works for data frames and matrices", {
  check(data.frame(), 'tab', 'csv', '')
  check(data.frame(a=1:3), 'tab', 'csv', 'a\n1\n2\n3')
  check(data.frame(a=1:3,b=c('x','y','z')), 'tab', 'csv', 'a,b\n1,x\n2,y\n3,z')

  check(matrix(), 'null', 'text', 'null')
  check(matrix(data=1:4,nrow=2), 'tab', 'csv', 'V1,V2\n1,3\n2,4')
})

test_that("the pack function works for recorded plots", {
  # For recodPlot to work..
  dev.new()
  dev.control('enable')

  plot(mpg~disp, mtcars)
  p <- pack(recordPlot())
  expect_equal(p$type, 'img')
  expect_equal(p$format, 'png')
  expect_equal(str_sub(p$value, 1, 15), 'data:image/png;')
})

if (require('ggplot2', quietly=T)) {
  test_that("the pack function works for ggplots", {
    p <- pack(ggplot(mtcars) + geom_point(aes(x=disp,y=mpg)))
    expect_equal(p$type, 'img')
    expect_equal(p$format, 'png')
    expect_equal(str_sub(p$value, 1, 15), 'data:image/png;')
  })
}
