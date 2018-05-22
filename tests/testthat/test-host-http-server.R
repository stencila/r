context("HostHttpServer")

test_that("HostHttpServer$stop+start", {
  s1 <- HostHttpServer$new(NULL)
  s2 <- HostHttpServer$new(NULL)

  expect_equal(s1$url, NULL)

  s1$start()
  expect_true(str_detect(s1$url, "^http://127.0.0.1"))

  s2$start()
  p1 <- as.integer(str_match(s1$url, "^http://127.0.0.1:(\\d+)")[1, 2])
  p2 <- as.integer(str_match(s2$url, "^http://127.0.0.1:(\\d+)")[1, 2])
  expect_true(p2 > p1)
  s2$stop()

  s1$stop()
  expect_equal(s1$url, NULL)
})

test_that("HostHttpServer$route", {
  s <- HostHttpServer$new(NULL)

  expect_equal(s$route("GET", "/"), c("static", "index.html"))
  expect_equal(s$route("GET", "/static/some/file.js"), c("static", "some/file.js"))

  # Unversioned requests

  expect_equal(s$route("GET", "/manifest"), c("run", "manifest"))
  expect_equal(s$route("POST", "/type", TRUE), c("run", "create", "type"))
  expect_equal(s$route("PUT", "/id!method", TRUE), c("run", "call", "id", "method"))
  expect_equal(s$route("DELETE", "/id", TRUE), c("run", "destroy", "id"))

  # v1 requests

  expect_equal(s$route("GET", "/v1/manifest", FALSE), c("run", "manifest"))

  expect_equal(s$route("GET", "/v1/services", TRUE), c("run", "services"))

  expect_equal(s$route("GET", "/v1/instances", TRUE), c("run", "instances"))
  expect_equal(s$route("GET", "/v1/instances", FALSE), "error_401")

  expect_equal(s$route("POST", "/v1/instances/Service", TRUE), c("run", "create", "Service"))
  expect_equal(s$route("POST", "/v1/instances/Service", FALSE), "error_401")

  expect_equal(s$route("DELETE", "/v1/instances/instance1", TRUE), c("run", "destroy", "instance1"))

  expect_equal(s$route("PUT", "/v1/instances/instance1/method", TRUE), c("run", "call", "instance1", "method"))
  expect_equal(s$route("PUT", "/v1/instances/instance1/method", FALSE), "error_401")

  expect_equal(s$route("PUT", "/v1/foobar", TRUE), "error_400")
})

test_that("HostHttpServer$handle", {
  skip("WIP")

  s <- HostHttpServer$new(host)

  r <- s$handle(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    HTTP_ACCEPT = "",
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$status, 403)

  # Authorization using a ticket
  r <- s$handle(list(
    PATH_INFO = "/",
    QUERY_STRING = paste0("?ticket=", s$ticket_create()),
    REQUEST_METHOD = "GET",
    HTTP_ACCEPT = "",
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$status, 200)
  expect_equal(str_sub(r$headers["Set-Cookie"], 1, 6), "token=")
  token <- str_match(r$headers["Set-Cookie"], "token=([a-zA-Z0-9]+);")[1, 2]
  expect_equal(str_sub(r$body, 1, 23), "<!doctype html>\n<html>\n")

  # Authorization using a token
  r <- s$handle(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    HTTP_ACCEPT = "application/json",
    HTTP_COOKIE = paste0("token=", token),
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$status, 200)
  expect_equal(str_sub(r$body, 1, 22), "{\"stencila\":{\"package\"")

  # Browser-based CORS request
  for (origin in c("http://127.0.0.1:2000", "http://localhost:2010", "https://open.stenci.la")) {
    r <- s$handle(list(
      PATH_INFO = "/",
      REQUEST_METHOD = "GET",
      HTTP_COOKIE = paste0("token=", token),
      HTTP_REFERER = sprintf("%s/some/file/path", origin),
      rook.input = list(read_lines = function() NULL)
    ))
    expect_equal(r$headers[["Access-Control-Allow-Origin"]], origin)
    expect_equal(r$headers[["Access-Control-Allow-Credentials"]], "true")
  }

  # Browser-based CORS pre-flight request
  for (origin in c("http://127.0.0.1:2000", "http://localhost:2010", "https://open.stenci.la")) {
    r <- s$handle(list(
      PATH_INFO = "/",
      REQUEST_METHOD = "OPTIONS",
      HTTP_ORIGIN = origin,
      rook.input = list(read_lines = function() NULL)
    ))
    expect_equal(r$headers[["Access-Control-Allow-Origin"]], origin)
    expect_equal(r$headers[["Access-Control-Allow-Credentials"]], "true")
    expect_equal(r$headers[["Access-Control-Allow-Methods"]], "GET, POST, PUT, DELETE, OPTIONS")
    expect_equal(r$headers[["Access-Control-Max-Age"]], "86400")
  }

  # Browser-based CORS pre-flight request from third party site
  r <- s$handle(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "OPTIONS",
    HTTP_ORIGIN = "http://evil.hackers.com",
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$headers[["Access-Control-Allow-Origin"]], NULL)
})

test_that("HostHttpServer$static", {
  s <- HostHttpServer$new(host)

  r <- s$static(list(), "logo-name-beta.svg")
  expect_equal(r$status, 200)
  expect_equal(r$headers[["Content-Type"]], "image/svg+xml")
  expect_equal(str_sub(r$body, 1, 54), "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")

  r <- s$static(list(), "foo.bar")
  expect_equal(r$status, 404)

  r <- s$static(list(), "../DESCRIPTION")
  expect_equal(r$status, 403)
})

test_that("HostHttpServer$call", {
  skip("WIP")

  s <- HostHttpServer$new(host)

  r <- s$post(list(), "RContext")
  expect_equal(r$status, 200)
  expect_equal(r$headers[["Content-Type"]], "application/json")

  r1 <- s$post(list(), "RContext")
  id <- from_json(r1$body)

  r2 <- s$get(list(), id)
  expect_equal(r2$status, 200)
  expect_equal(r2$headers[["Content-Type"]], "application/json")
  expect_equal(r2$body, "{}")

  r1 <- s$post(list(), "RContext")
  id <- from_json(r1$body)

  r2 <- s$put(list(body = "\"6*7\""), id, "execute")
  expect_equal(r2$status, 200)
  expect_equal(r2$headers[["Content-Type"]], "application/json")
  expect_equal(from_json(r2$body)$type, "cell")
})
