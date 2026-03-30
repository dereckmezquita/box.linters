# nolint start: line_length_linter
#' R6 class usage linter
#'
#' Checks method and attribute calls within an R6 class. Covers public, private, and active objects.
#' All internal calls should exist. All private methods and attributes should be used.
#'
#' For use in `rhino`, see the
#' [Explanation: Rhino style guide](https://appsilon.github.io/rhino/articles/explanation/rhino-style-guide.html)
#' to learn about the details.
#'
#' @return A custom linter function for use with `r-lib/lintr`.
#'
#' @examples
#' # will produce lints
#' code = "
#' box::use(
#'   R6[R6Class],
#' )
#'
#' badClass <- R6Class('badClass',
#'   public = list(
#'     initialize = function() {
#'       private$not_exists()
#'     }
#'   ),
#'   private = list(
#'     unused_attribute = 'private data',
#'     unused_method = function() {
#'       self$attribute_not_exists
#'       self$function_not_exists()
#'     }
#'   )
#' )
#' "
#'
#' lintr::lint(
#'   text = code,
#'   linters = r6_usage_linter()
#' )
#'
#' # okay
#' code = "
#' box::use(
#'   R6[R6Class],
#' )
#'
#' goodClass <- R6Class('goodClass',
#'   public = list(
#'     public_attr = NULL,
#'     initialize = function() {
#'       private$private_func()
#'     },
#'     some_function = function () {
#'       private$private_attr
#'     }
#'   ),
#'   private = list(
#'     private_attr = 'private data',
#'     private_func = function() {
#'       self$public_attr
#'     }
#'   )
#' )
#' "
#'
#' lintr::lint(
#'   text = code,
#'   linters = r6_usage_linter()
#' )
#'
#' @export
# nolint end
r6_usage_linter <- function() {
  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "file")) {
      return(list())
    }

    full_xml <- source_expression$full_xml_parsed_content

    xpath_r6_class <- "
    //expr[
      expr[1]/SYMBOL_FUNCTION_CALL[text() = 'R6Class']
    ]"

    r6_classes <- xml2::xml_find_all(full_xml, xpath_r6_class)

    if (length(r6_classes) == 0) {
      return(list())
    }

    all_lints <- lapply(r6_classes, function(r6_class) {
      public_components <- get_r6_components(r6_class, "public")
      active_components <- get_r6_components(r6_class, "active")
      private_components <- get_r6_components(r6_class, "private")

      inherited_components <- get_r6_inherited_components(r6_class, full_xml)

      components_text <- c(
        public_components$text,
        active_components$text,
        private_components$text,
        inherited_components
      )

      xpath_self_calls <- make_r6_internal_calls_xpath("self")
      xpath_private_calls <- make_r6_internal_calls_xpath("private")

      self_method_calls <- extract_xml_and_text(r6_class, xpath_self_calls)
      private_method_calls <- extract_xml_and_text(r6_class, xpath_private_calls)

      all_internal_calls <- c(self_method_calls$xml, private_method_calls$xml)
      all_internal_call_names <- c(self_method_calls$text, private_method_calls$text)

      invalid_object <- lapply(all_internal_calls, function(internal_call) {
        call_name <- lintr::get_r_string(internal_call)

        if (!call_name %in% components_text) {
          lintr::xml_nodes_to_lints(
            internal_call,
            source_expression = source_expression,
            lint_message = "Internal object call not found.",
            type = "warning"
          )
        }
      })

      unused_private <- lapply(private_components$xml, function(component) {
        component_name <- paste(
          "private",
          lintr::get_r_string(component),
          sep = "$"
        )

        if (!component_name %in% all_internal_call_names) {
          lintr::xml_nodes_to_lints(
            component,
            source_expression = source_expression,
            lint_message = "Private object not used.",
            type = "warning"
          )
        }
      })

      c(invalid_object, unused_private)
    })

    unlist(all_lints, recursive = FALSE)
  })
}

#' XPath to get internal components of an R6 class
#'
#' @param mode Type of internal component (`public`, `active`, or `private`).
#' @return An XPath query
#' @keywords internal
make_r6_components_xpath <- function(mode = c("public", "active", "private")) {
  glue::glue("
    .//SYMBOL_SUB[text() = '{mode}']
    /following-sibling::EQ_SUB[1]
    /following-sibling::expr[1]
    /child::SYMBOL_SUB
  ")
}

#' XPath to get internal function or data object calls inside an R6 class
#'
#' @param mode Type of internal call (`self` or `private`).
#' @return An XPath query
#' @keywords internal
make_r6_internal_calls_xpath <- function(mode = c("self", "private")) {
  glue::glue("
    .//expr[
      ./expr/SYMBOL[text() = '{mode}'] and
      ./OP-DOLLAR and
      (
        ./SYMBOL_FUNCTION_CALL or
        ./SYMBOL
      )
    ]
  ")
}

#' Get declared/defined R6 class components
#'
#' @param xml XML representation of R source code.
#' @param mode Type of internal component (`public`, `active`, `private`).
#' @return List of XML nodes and corresponding text string values of the nodes
#' @keywords internal
#' Get components inherited from parent R6 classes
#'
#' Looks for `inherit = ParentClass` in the R6Class call, finds
#' the parent class definition in the same file, and extracts its
#' public, active, and private components. Recurses for grandparents.
#'
#' @param r6_class XML node of the R6Class call being checked.
#' @param full_xml Full file XML to search for parent class definitions.
#' @param visited Character vector of already-visited class names (cycle guard).
#' @return Character vector of inherited component names (e.g., "self$log", "private$ctx")
#' @keywords internal
get_r6_inherited_components <- function(r6_class, full_xml, visited = character()) {
  xpath_inherit <- ".//SYMBOL_SUB[text() = 'inherit']/following-sibling::EQ_SUB[1]/following-sibling::expr[1]/SYMBOL"
  parent_nodes <- xml2::xml_find_all(r6_class, xpath_inherit)

  if (length(parent_nodes) == 0) {
    return(character())
  }

  parent_name <- xml2::xml_text(parent_nodes[[1]])

  if (parent_name %in% visited) {
    return(character())
  }

  xpath_parent_assign <- glue::glue(
    "//expr[LEFT_ASSIGN and expr/SYMBOL[text() = '{parent_name}']]"
  )
  parent_assign <- xml2::xml_find_all(full_xml, xpath_parent_assign)

  parent_class <- xml2::xml_find_all(
    parent_assign,
    ".//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'R6Class']]"
  )

  if (length(parent_class) == 0) {
    return(character())
  }

  parent_public <- get_r6_components(parent_class[[1]], "public")
  parent_active <- get_r6_components(parent_class[[1]], "active")
  parent_private <- get_r6_components(parent_class[[1]], "private")

  parent_components <- c(parent_public$text, parent_active$text, parent_private$text)

  grandparent_components <- get_r6_inherited_components(
    parent_class[[1]],
    full_xml,
    visited = c(visited, parent_name)
  )

  return(c(parent_components, grandparent_components))
}

get_r6_components <- function(xml, mode = c("public", "active", "private")) {
  xpath <- make_r6_components_xpath(mode)
  components <- xml2::xml_find_all(xml, xpath)

  base_prefix <- list(
    "public" = "self",
    "active" = "self",
    "private" = "private"
  )

  components_text <- unlist(
    lapply(components, function(component) {
      component_name <- lintr::get_r_string(component)

      paste(
        base_prefix[[mode]],
        component_name,
        sep = "$"
      )
    })
  )

  list(
    xml = components,
    text = components_text
  )
}
