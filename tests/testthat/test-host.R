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

  it("has a post() method", {
    id1 <- host$post("RContext")
    id2 <- host$post("RContext")
    expect_true(id1 != id2)

    expect_error(host$post("Foo"), "Unknown type")
  })

  it("has a get() method", {
    id <- host$post("RContext")
    expect_true(inherits(host$get(id), "RContext"))

    expect_error(host$get("foo"), "Unknown instance")
  })

  it("has a put() method", {
    id <- host$post("RContext")
    expect_equal(host$put(id, "execute", "6*7")$type, "cell")
    expect_error(host$put(id, "fooBar"), "Unknown method")
    expect_error(host$put("foo", "bar"), "Unknown instance")
  })

  it("has a delete() method", {
    id <- host$post("RContext")
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
    expect_true(host$authorize_token(token1))
    Sys.sleep(1)
    token2 <- host$generate_token()
    expect_true(host$authorize_token(token2))
    expect_false(token1 == token2)
  })
})
