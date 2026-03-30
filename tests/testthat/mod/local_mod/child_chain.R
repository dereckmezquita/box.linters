box::use(R6)
box::use(p = ./parent_cross)

Child <- R6$R6Class("Child",
  inherit = p$ParentCross,
  public = list(
    run = function() {
      self$parent_method()
      self$ancient_method()
    }
  )
)
