box::use(R6)
box::use(gp = ./grandparent)

#' @export
ParentCross <- R6$R6Class("ParentCross",
  inherit = gp$GrandParent,
  public = list(
    parent_method = function() { }
  )
)
