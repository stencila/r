context("Host")

describe("Host", {
  host <- Host$new()

  it("is a class", {
    expect_equal(class(host)[1], "Host")
  })

  it("has a manifest() method", {
    manifest <- host$manifest()
    expect_equal(manifest$stencila$package, "r")
    expect_equal(manifest$stencila$version, version)
    expect_equal(length(manifest$urls), 0)
    expect_equal(length(manifest$instances), 0)
    expect_true(length(manifest$types) > 0)
  })

  it("has a register() method", {
    host$register()
    manifest <- host$manifest(complete = FALSE)
    expect_equal(
      manifest,
      from_json(file.path(host$user_dir(), "hosts", "r.json"))
    )
  })

  it("has a create() method", {
    id1 <- host$create("RContext")
    id2 <- host$create("RContext")
    expect_true(id1 != id2)

    expect_error(host$create("Foo"), "Unknown type")
  })

  it("has a get() method", {
    id <- host$create("RContext")
    expect_true(inherits(host$get(id), "RContext"))

    expect_error(host$get("foo"), "Unknown instance")
  })

  it("has a call() method", {
    id <- host$create("RContext")
    expect_equal(host$call(id, "execute", "6*7")$type, "cell")
    expect_error(host$call(id, "fooBar"), "Unknown method")
    expect_error(host$call("foo", "bar"), "Unknown instance")
  })

  it("has a delete() method", {
    id <- host$create("RContext")
    expect_true(inherits(host$get(id), "RContext"))
    host$delete(id)
    expect_error(host$delete(id), "Unknown instance")
  })

  it("has start() and stop() methods", {
    host$start(quiet = TRUE)
    expect_equal(names(host$servers), "http")
    expect_equal(length(host$servers), 1)
    expect_equal(length(host$manifest()$servers), 1)
    expect_true(file.exists(host$run_file()))
    host$stop()
    expect_equal(length(host$servers), 0)
    expect_equal(length(host$manifest()$servers), 0)
    expect_true(!file.exists(host$run_file()))
  })

  it("has generate_token() and authorize_token() methods", {
    token1 <- host$generate_token()
    host$authorize_token(token1)

    token2 <- host$generate_token()
    host$authorize_token(token2)

    expect_true(token1 != token2)
    expect_error(host$authorize_token("not a valid token"))
    expect_error(host$authorize_token("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpYXQiOjE1MjY5NjA1Nzl9.pgTAtdDGHZZd05hg-Tmy8Cl_yrWBzBSZMaCTkbztc1E"))
  })
})
