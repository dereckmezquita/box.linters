box::use(R6)
box::use(bf = ./base_class)

Child <- R6$R6Class("Child",
  inherit = bf$BaseClass,
  public = list(
    run = function() {
      self$base_method()
      self$base_attr
    }
  )
)
