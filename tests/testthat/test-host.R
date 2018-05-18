describe("Host", {
  h <- Host$new()

  it("is a class", {
    expect_equal(class(h)[1], "Host")
  })

  it("has an manifest() method", {
    manifest <- h$manifest()
    expect_equal(manifest$stencila$package, "r")
    expect_equal(manifest$stencila$version, version)
    expect_equal(length(manifest$urls), 0)
    expect_equal(length(manifest$instances), 0)
    expect_true(length(manifest$schemes) > 0)
  })

  it("has an install() method", {
    h$install()
    manifest <- h$manifest(complete = FALSE)
    expect_equal(
      manifest,
      fromJSON(file.path(h$user_dir(), "hosts", "r.json"))
    )
  })

  it("has a post() method", {
    id1 <- h$post("RContext")
    id2 <- h$post("RContext")
    expect_true(id1 != id2)

    expect_error(h$post("Foo"), "Unknown type")
  })

  it("has a get() method", {
    id <- h$post("RContext")
    expect_true(inherits(h$get(id), "RContext"))

    expect_error(h$get("foo"), "Unknown instance")
  })

  it("has a put() method", {
    id <- h$post("RContext")
    expect_equal(h$put(id, "executeCode", list(code = "6*7"))$value$data, 42)
    expect_error(h$put(id, "fooBar"), "Unknown method")
    expect_error(h$put("foo", "bar"), "Unknown instance")
  })

  it("has a delete() method", {
    id <- h$post("RContext")
    expect_true(inherits(h$get(id), "RContext"))
    h$delete(id)
    expect_error(h$delete(id), "Unknown instance")
  })

  it("has start() and stop() methods", {
    h$start(quiet = TRUE)
    expect_equal(names(h$servers), "http")
    expect_equal(length(h$servers), 1)
    expect_equal(length(h$manifest()$urls), 1)
    expect_true(file.exists(h$run_file()))
    h$stop()
    expect_equal(length(h$servers), 0)
    expect_equal(length(h$manifest()$urls), 0)
    expect_true(!file.exists(h$run_file()))
  })
})
