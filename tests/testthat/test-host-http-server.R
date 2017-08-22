test_that("HostHttpServer$stop+start", {
  s1 = HostHttpServer$new(NULL)
  s2 = HostHttpServer$new(NULL)

  expect_equal(s1$url, NULL)

  s1$start()
  expect_true(str_detect(s1$url, '^http://127.0.0.1'))

  s2$start()
  p1 <- as.integer(str_match(s1$url, '^http://127.0.0.1:(\\d+)')[1, 2])
  p2 <- as.integer(str_match(s2$url, '^http://127.0.0.1:(\\d+)')[1, 2])
  expect_equal(p2, p1+10)
  s2$stop()

  # Unfortunately this timesout here. But will work from a separate R process.
  #r = GET(s$origin, timeout(10))

  s1$stop()
  expect_equal(s1$url, NULL)

})

test_that("HostHttpServer.route", {
  s = HostHttpServer$new(NULL)

  expect_equal(s$route('OPTIONS', NULL), list(s$options))

  expect_equal(s$route('GET', '/'), list(s$home))

  expect_equal(s$route('GET', '/static/some/file.js'), list(s$static, 'some/file.js'))
  expect_equal(s$route('GET', '/favicon.ico'), list(s$static, 'favicon.ico'))

  expect_equal(s$route('POST', '/type'), list(s$post, 'type'))

  expect_equal(s$route('GET', '/id'), list(s$get, 'id'))

  expect_equal(s$route('PUT', '/id!method'), list(s$put, 'id', 'method'))

  expect_equal(s$route('DELETE', '/id'), list(s$delete, 'id'))
})

test_that("HostHttpServer.handle", {
  s = HostHttpServer$new(host)

  r <- s$handle(list(
    PATH_INFO = '/',
    REQUEST_METHOD = 'GET',
    HTTP_ACCEPT = '',
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$status, 200)
  expect_equal(str_sub(r$body, 1, 23), '<!doctype html>\n<html>\n')

  r <- s$handle(list(
    PATH_INFO = '/',
    REQUEST_METHOD = 'GET',
    HTTP_ACCEPT = 'application/json',
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$status, 200)
  expect_equal(str_sub(r$body, 1, 22), '{"stencila":{"package"')

  # Browser-based CORS request
  for (origin in c('http://127.0.0.1:2000', 'http://localhost:2010', 'https://open.stenci.la')) {
    r <- s$handle(list(
      PATH_INFO = '/',
      REQUEST_METHOD = 'GET',
      HTTP_REFERER = sprintf('%s/some/file/path', origin),
      rook.input = list(read_lines = function() NULL)
    ))
    expect_equal(r$headers[['Access-Control-Allow-Origin']], origin)
    expect_equal(r$headers[['Access-Control-Allow-Credentials']], 'true')
  }

  # Browser-based CORS pre-flight request
  for (origin in c('http://127.0.0.1:2000', 'http://localhost:2010', 'https://open.stenci.la')) {
    r <- s$handle(list(
      PATH_INFO = '/',
      REQUEST_METHOD = 'OPTIONS',
      HTTP_ORIGIN = origin,
      rook.input = list(read_lines = function() NULL)
    ))
    expect_equal(r$headers[['Access-Control-Allow-Origin']], origin)
    expect_equal(r$headers[['Access-Control-Allow-Credentials']], 'true')
    expect_equal(r$headers[['Access-Control-Allow-Methods']], 'GET, POST, PUT, DELETE, OPTIONS')
    expect_equal(r$headers[['Access-Control-Max-Age']], '86400')
  }

  # Browser-based CORS pre-flight request from third party site
  r <- s$handle(list(
    PATH_INFO = '/',
    REQUEST_METHOD = 'OPTIONS',
    HTTP_ORIGIN = 'http://evil.hackers.com',
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$headers[['Access-Control-Allow-Origin']], NULL)
})

test_that("HostHttpServer.options", {
  s = HostHttpServer$new(host)

  r = s$options()
  expect_equal(r$status, 200)
  expect_equal(r$body, '')
})

test_that("HostHttpServer.home", {
  s = HostHttpServer$new(host)

  r = s$home(list(headers=list('Accept'='application/json')))
  expect_equal(r$status, 200)
  expect_equal(fromJSON(r$body)$stencila, host$manifest()$stencila)

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
  expect_equal(r$headers[['Content-Type']], 'application/json')
})

test_that("HostHttpServer.get", {
  s = HostHttpServer$new(host)

  r1 = s$post(list(), 'RContext')
  r2 = s$get(list(), fromJSON(r1$body))
  expect_equal(r2$status, 200)
  expect_equal(r2$headers[['Content-Type']], 'application/json')
  expect_equal(r2$body, '{}')
})

test_that("HostHttpServer.put", {
  s = HostHttpServer$new(host)

  r1 = s$post(list(), 'RContext')
  id = fromJSON(r1$body)
  r2 = s$put(list(body='{"code":"6*7"}'), id, 'runCode')
  expect_equal(r2$status, 200)
  expect_equal(r2$headers[['Content-Type']], 'application/json')
  expect_equal(fromJSON(r2$body)$output$content, '42')
})
