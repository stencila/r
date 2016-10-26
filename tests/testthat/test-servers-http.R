test_that("HttpServer can serve", {
  s = HttpServer$new(NULL)

  s$serve()
  expect_equal(s$status, 'on')
  expect_true(str_detect(s$url,'^http://127.0.0.1'))

  # Unfortunately this timesout here. But will work from a separate R process.
  #r = GET(s$origin, timeout(10))

  s$serve(FALSE)
  expect_equal(s$status, 'off')
})

test_that("HttpServer routes to endpoints correctly", {
  s = HttpServer$new(NULL)

  expect_equal(s$route('EGT', '/web/some/file.js'), list(s$web, 'some/file.js'))
  expect_equal(s$route('GET', '/favicon.ico'), list(s$web, 'images/favicon.ico'))

  expect_equal(s$route('GET', '/!name'), list(s$get, NULL, 'name'))
  expect_equal(s$route('GET', '/some/address!name'), c(s$get, 'some/address', 'name'))

  expect_equal(s$route('PUT', '/!name'), list(s$set, NULL, 'name'))
  expect_equal(s$route('PUT', '/some/address!name'), c(s$set, 'some/address', 'name'))

  expect_equal(s$route('POST', '/!method'), list(s$call, NULL, 'method'))
  expect_equal(s$route('POST', '/git://some/address!method'), list(s$call, 'git://some/address', 'method'))

  expect_equal(s$route('GET', '/'), list(s$show, ''))
  expect_equal(s$route('GET', '/file://some/address'), list(s$show, 'file://some/address'))
})

test_that("HttpServer web method works", {
  s = HttpServer$new(host)
  r = s$web(list(), 'some/file.js')
  expect_equal(r$status, 302)
  expect_true(!is.na(r$headers[['Location']]))
})

test_that("HttpServer get method works", {
  s = HttpServer$new(host)
  c = RSession$new()

  r = s$get(list(), c$address, 'type')
  expect_equal(r$status, 200)
  expect_equal(r$body, '"session-r"')
})

test_that("HttpServer call component method works", {
  s = HttpServer$new(host)
  c = RSession$new()

  r = s$call(list(body='{"expr":"6*7"}'), c$address, 'print')
  expect_equal(r$status, 200)
  expect_equal(r$body, '"42"')
})

test_that("HttpServer call host method works", {
  s = HttpServer$new(host)

  r = s$call(list(body='{"manifest":{}}'), NULL, 'hello')
  expect_equal(r$status, 200)
  expect_equal(fromJSON(r$body)$stencila, TRUE)
})
