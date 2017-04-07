describe("Host", {
  h <- Host$new()

  it("is a class", {
    expect_equal(class(h)[1], "Host")
  })

  it('has an options method', {
    manifest <- h$options()
    expect_equal(manifest$stencila$package, 'r')
    expect_equal(manifest$stencila$version, version)
    expect_equal(length(manifest$urls), 0)
    expect_equal(length(manifest$instances), 0)
    expect_true(length(manifest$types) > 0)
  })

  it('has a post() method', {
    id1 <- h$post('RContext')
    id2 <- h$post('RContext')
    expect_true(id1 != id2)

    expect_error(h$post('Foo'), 'Unknown type')
  })

  it('has a get() method', {
    id <- h$post('RContext')
    expect_true(inherits(h$get(id), 'RContext'))
    
    expect_error(h$get('foo'), 'Unknown instance')
  })

  it('has a put() method', {
    id <- h$post('RContext')
    expect_equal(h$put(id, 'runCode', list(code='6*7'))$output, pack(42))
    expect_error(h$put(id, 'fooBar'), 'Unknown method')
    expect_error(h$put('foo', 'bar'), 'Unknown instance')
  })

  it('has a delete() method', {
    id <- h$post('RContext')
    expect_true(inherits(h$get(id), 'RContext'))
    h$delete(id)
    expect_error(h$delete(id), 'Unknown instance')
  })

  it('has a start() and stop() methods', {
    h$start()
    expect_equal(h$servers, 'http')
    expect_equal(length(h$servers), 1)
    expect_equal(length(h$options()$urls), 1)
    h$stop()
    expect_equal(length(h$servers), 0)
    expect_equal(length(h$options()$urls), 0)
  })
})
