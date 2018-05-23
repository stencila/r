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

  expect_equal(s$route("POST", "/environ/local", TRUE), c("run", "startup", "local"))
  expect_equal(s$route("DELETE", "/environ/id", TRUE), c("run", "shutdown", "id"))

  expect_equal(s$route("POST", "/type", TRUE), c("run", "create", "type"))
  expect_equal(s$route("PUT", "/id!method", TRUE), c("run", "call", "id", "method"))
  expect_equal(s$route("DELETE", "/id", TRUE), c("run", "destroy", "id"))

  # v1 requests

  expect_equal(s$route("GET", "/v1/manifest", FALSE), c("run", "manifest"))
  expect_equal(s$route("GET", "/v1/environs", FALSE), c("run", "environs"))
  expect_equal(s$route("GET", "/v1/services", FALSE), c("run", "services"))

  expect_equal(s$route("GET", "/v1/hosts", TRUE), c("run", "hosts"))
  expect_equal(s$route("POST", "/v1/hosts/local", TRUE), c("run", "startup", "local"))
  expect_equal(s$route("DELETE", "/v1/hosts/id", TRUE), c("run", "shutdown", "id"))
  expect_equal(s$route("GET", "/v1/hosts", FALSE), c("error_401", "/v1/hosts"))

  expect_equal(s$route("GET", "/v1/instances", TRUE), c("run", "instances"))
  expect_equal(s$route("GET", "/v1/instances", FALSE), c("error_401", "/v1/instances"))

  expect_equal(s$route("POST", "/v1/instances/Service", TRUE), c("run", "create", "Service"))
  expect_equal(s$route("POST", "/v1/instances/Service", FALSE), c("error_401", "/v1/instances/Service"))

  expect_equal(s$route("DELETE", "/v1/instances/instance1", TRUE), c("run", "destroy", "instance1"))

  expect_equal(s$route("PUT", "/v1/instances/instance1/method", TRUE), c("run", "call", "instance1", "method"))
  expect_equal(s$route("PUT", "/v1/instances/instance1/method", FALSE), c("error_401", "/v1/instances/instance1/method"))

  expect_equal(s$route("PUT", "/v1/foobar", TRUE), c("error_400", "/v1/foobar"))
})

test_that("HostHttpServer$handle", {
  s <- HostHttpServer$new(host)

  # Home
  r <- s$handle(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$status, 200)
  expect_equal(str_sub(r$body, 1, 23), "<!doctype html>\n<html>\n")

  # Manifest
  r <- s$handle(list(
    PATH_INFO = "/v1/manifest",
    REQUEST_METHOD = "GET",
    rook.input = list(read_lines = function() NULL)
  ))
  expect_equal(r$status, 200)
  expect_equal(str_sub(r$body, 1, 22), "{\"stencila\":{\"package\"")

  # Browser-based CORS request
  for (origin in c("http://127.0.0.1:2000", "http://localhost:2010", "https://open.stenci.la")) {
    r <- s$handle(list(
      PATH_INFO = "/v1/manifest",
      REQUEST_METHOD = "GET",
      HTTP_REFERER = sprintf("%s/some/file/path", origin),
      rook.input = list(read_lines = function() NULL)
    ))
    expect_equal(r$headers[["Access-Control-Allow-Origin"]], origin)
    expect_equal(r$headers[["Access-Control-Allow-Credentials"]], "true")
  }

  # Browser-based CORS pre-flight request
  for (origin in c("http://127.0.0.1:2000", "http://localhost:2010", "https://open.stenci.la")) {
    r <- s$handle(list(
      PATH_INFO = "/v1/manifest",
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

test_that("HostHttpServer$run", {
  s <- HostHttpServer$new(host)
  req <- list()

  # Get manifest
  r <- s$run(req, "manifest")
  expect_equal(r$status, 200)
  expect_equal(r$headers[["Content-Type"]], "application/json")

  # Create an RContext
  r <- s$run(req, "create", "RContext")
  expect_equal(r$status, 200)
  expect_equal(r$headers[["Content-Type"]], "application/json")
  id <- from_json(r$body)

  # Call a context method
  r <- s$run(list(body = "\"6*7\""), "call", id, "execute")
  expect_equal(r$status, 200)
  expect_equal(r$headers[["Content-Type"]], "application/json")
  cell <- from_json(r$body)
  expect_equal(cell$type, "cell")

  # Delete the context
  r <- s$run(req, "delete", id)
  expect_equal(r$status, 200)
  expect_equal(r$headers[["Content-Type"]], "application/json")
})
