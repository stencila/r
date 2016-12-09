Session <- R6Class("Session",
  inherit = Component,

  public = list(

    initialize = function () {
      super$initialize()
    }

  ),

  active = list(

    kind = function () {
      'session'
    }

  ),

  private = list(
  )
)
