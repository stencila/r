# A component class with a property foo that can be set for testing the 
# `.set` method
ComponentDerived <- R6Class("ComponentDerived",
  inherit = Component,
  public = list(
    initialize = function () {
      super$initialize()
      foo <- NULL
    },
    foo = NULL
  )
)

describe('ComponentProxy', {
  c <- ComponentDerived$new()
  host$serve()
  p <- ComponentProxy(c$type, c$url)

  it('has the correct properties', {
    expect_equal(class(p)[1], "ComponentProxy")
    expect_equal(p$type, c$type)
    expect_equal(p$url, c$url)
    expect_equal(p$address, c$address)
  })

  it('has a dump() method', {
    expect_equal(p$dump(), list(type=c$type, id=c$id, address=c$address, url=c$url))
    expect_equal(p$dump('json'), toJSON(p$dump(), auto_unbox=TRUE))
  })

  it('has a .get method and a getter', {
    expect_equal(p[['.get']]('id'),c$id)
    expect_equal(p$id,c$id)
  })

  it('has a .set method and a setter', {
    p[['.set']]('foo', 'bar')
    expect_equal(c$foo, 'bar')

    p$foo <- 'bart'
    expect_equal(c$foo, 'bart')
  })

  it('has a call method', {
    expect_equal(p[['.call']]('short'), c$short())
    expect_equal(p[['.call']]('short', 'name://fooby'), c$short('name://fooby'))
  })
})

