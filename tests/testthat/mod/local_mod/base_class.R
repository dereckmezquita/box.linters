box::use(R6)

#' @export
BaseClass <- R6$R6Class("BaseClass",
  public = list(
    base_method = function() { },
    base_attr = NULL
  ),
  private = list(
    base_secret = NULL
  )
)
