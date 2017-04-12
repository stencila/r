test_that("HostHttpServer$stop+start", {
  s = HostHttpServer$new(NULL)

  expect_equal(s$url, NULL)

  s$start()
  expect_true(str_detect(s$url,'^http://127.0.0.1'))

  # Unfortunately this timesout here. But will work from a separate R process.
  #r = GET(s$origin, timeout(10))

  s$stop()
  expect_equal(s$url, NULL)
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
  s = HostHttpServer$new(host)

  r = s$home(list(headers=list('Accept'='application/json')))
  expect_equal(r$status, 200)
  expect_equal(fromJSON(r$body), host$options())

  r = s$home(list())
  expect_equal(r$status, 200)
  expect_equal(r$headers[['Content-Type']], 'text/html')
})

test_that("HostHttpServer.static", {
  s = HostHttpServer$new(host)

  r = s$static(list(), 'logo-name-beta.svg')
  expect_equal(r$status, 200)
  expect_equal(r$headers[['Content-Type']], 'image/svg+xml')
  expect_equal(str_sub(r$body, 1, 54), '<?xml version="1.0" encoding="UTF-8" standalone="no"?>')

  r = s$static(list(), 'foo.bar')
  expect_equal(r$status, 404)

  r = s$static(list(), '../DESCRIPTION')
  expect_equal(r$status, 403)
})

test_that("HostHttpServer.post", {
  s = HostHttpServer$new(host)

  r = s$post(list(), 'RContext')
  expect_equal(r$status, 200)
})

test_that("HostHttpServer.get", {
  s = HostHttpServer$new(host)

  r1 = s$post(list(), 'RContext')
  r2 = s$get(list(), r1$body)
  expect_equal(r2$status, 200)
  expect_equal(r2$body, '"r-host"')
})

test_that("HostHttpServer.put", {
  skip('Refactoring API')

  s = HostHttpServer$new(host)
  c = RContext$new()

  r = s$call(list(body='{"code":"6*7"}'), c$address, 'run')
  #expect_equal(r$status, 200)
  #expect_equal(fromJSON(r$body)$output$value, "42")
})
