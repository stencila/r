test_that("HostHttpServer$stop+start", {
  s = HostHttpServer$new(NULL)

  s$start()
  expect_equal(s$status, 'on')
  expect_true(str_detect(s$url,'^http://127.0.0.1'))

  # Unfortunately this timesout here. But will work from a separate R process.
  #r = GET(s$origin, timeout(10))

  s$stop()
  expect_equal(s$status, 'off')
})

test_that("HostHttpServer.route", {
  s = HostHttpServer$new(NULL)

  expect_equal(s$route('GET', '/'), list(s$home))

  expect_equal(s$route('GET', '/static/some/file.js'), list(s$static, 'some/file.js'))
  expect_equal(s$route('GET', '/favicon.ico'), list(s$static, 'favicon.ico'))

  expect_equal(s$route('POST', '/type'), list(s$post, 'type'))

  expect_equal(s$route('GET', '/id'), list(s$get, 'id'))

  expect_equal(s$route('PUT', '/id!method'), list(s$put, 'id', 'method'))

  expect_equal(s$route('DELETE', '/id'), list(s$delete, 'id'))
})

test_that("HostHttpServer.home", {
  skip('Refactoring API')

  s = HostHttpServer$new(host)

  r = s$call(list(body='{"manifest":{}}'), NULL, 'hello')
  expect_equal(r$status, 200)
  expect_equal(fromJSON(r$body)$stencila, TRUE)
})

test_that("HostHttpServer.static", {
  skip('Refactoring API')

  s = HostHttpServer$new(host)
  r = s$static(list(), 'some/file.js')
  expect_equal(r$status, 200)
  expect_true(!is.na(r$headers[['Location']]))
})

test_that("HostHttpServer.get", {
  skip('Refactoring API')

  s = HostHttpServer$new(host)
  c = RContext$new()

  r = s$get(list(), c$address, 'type')
  expect_equal(r$status, 200)
  expect_equal(r$body, '"r-host"')
})

test_that("HostHttpServer.put", {
  skip('Refactoring API')

  s = HostHttpServer$new(host)
  c = RContext$new()

  r = s$call(list(body='{"code":"6*7"}'), c$address, 'run')
  #expect_equal(r$status, 200)
  #expect_equal(fromJSON(r$body)$output$value, "42")
})
