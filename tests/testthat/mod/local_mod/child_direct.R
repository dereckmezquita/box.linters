box::use(R6)
box::use(./base_class[BaseClass])

Child <- R6$R6Class("Child",
  inherit = BaseClass,
  public = list(
    run = function() {
      self$base_method()
      self$base_attr
    }
  )
)
