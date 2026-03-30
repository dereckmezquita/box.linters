# nolint start: line_length_linter
#' R6 class usage linter
#'
#' Checks method and attribute calls within an R6 class. Covers public, private, and active objects.
#' All internal calls should exist. All private methods and attributes should be used.
#'
#' Supports R6 class inheritance via `inherit`. Inherited components from parent classes are
#' recognized as valid calls in child classes. Resolution works for:
#' * Same-file parent classes (including multi-level chains)
#' * Cross-file parent classes imported via `box::use()` (alias and direct import patterns)
#' * Cross-file chain inheritance (A -> B -> C across multiple files)
#' * Parent classes from installed packages (via R6ClassGenerator introspection)
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

      inherited_components <- get_r6_inherited_components(
        r6_class, full_xml,
        source_file = source_expression$filename
      )

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
get_r6_inherited_components <- function(r6_class, full_xml, source_file = NULL,
                                        visited = character()) {
  # Get the inherit expr node
  xpath_inherit_expr <- paste0(
    ".//SYMBOL_SUB[text() = 'inherit']",
    "/following-sibling::EQ_SUB[1]",
    "/following-sibling::expr[1]"
  )
  inherit_expr <- xml2::xml_find_all(r6_class, xpath_inherit_expr)

  if (length(inherit_expr) == 0) {
    return(character())
  }

  # Determine if inherit = ClassName or inherit = module$ClassName
  has_dollar <- length(xml2::xml_find_all(inherit_expr[[1]], "./OP-DOLLAR")) > 0

  if (has_dollar) {
    # Pattern: inherit = module_alias$ClassName
    symbols <- xml2::xml_find_all(inherit_expr[[1]], "./SYMBOL | ./expr/SYMBOL")
    if (length(symbols) < 2) {
      return(character())
    }
    module_alias <- xml2::xml_text(symbols[[1]])
    class_name <- xml2::xml_text(symbols[[2]])
  } else {
    # Pattern: inherit = ClassName
    class_symbols <- xml2::xml_find_all(inherit_expr[[1]], "./SYMBOL")
    if (length(class_symbols) == 0) {
      return(character())
    }
    module_alias <- NULL
    class_name <- xml2::xml_text(class_symbols[[1]])
  }

  if (class_name %in% visited) {
    return(character())
  }

  # 1. Try same-file lookup
  components <- resolve_r6_from_same_file(class_name, full_xml)
  if (!is.null(components)) {
    # Recurse for grandparent (same file)
    xpath_parent_assign <- glue::glue(
      "//expr[LEFT_ASSIGN and expr/SYMBOL[text() = '{class_name}']]"
    )
    parent_assign <- xml2::xml_find_all(full_xml, xpath_parent_assign)
    parent_r6 <- xml2::xml_find_all(
      parent_assign,
      ".//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'R6Class']]"
    )
    grandparent <- character()
    if (length(parent_r6) > 0) {
      grandparent <- get_r6_inherited_components(
        parent_r6[[1]], full_xml, source_file,
        visited = c(visited, class_name)
      )
    }
    return(c(components, grandparent))
  }

  # 2. Try resolving from box::use imports (module or package)
  components <- resolve_r6_from_imports(
    class_name, module_alias, full_xml, source_file,
    visited = c(visited, class_name)
  )
  return(components)
}

#' Find an R6 class in the same file and extract its components
#' @keywords internal
resolve_r6_from_same_file <- function(class_name, full_xml) {
  xpath_parent_assign <- glue::glue(
    "//expr[LEFT_ASSIGN and expr/SYMBOL[text() = '{class_name}']]"
  )
  parent_assign <- xml2::xml_find_all(full_xml, xpath_parent_assign)
  parent_class <- xml2::xml_find_all(
    parent_assign,
    ".//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'R6Class']]"
  )

  if (length(parent_class) == 0) {
    return(NULL)
  }

  parent_public <- get_r6_components(parent_class[[1]], "public")
  parent_active <- get_r6_components(parent_class[[1]], "active")
  parent_private <- get_r6_components(parent_class[[1]], "private")

  return(c(parent_public$text, parent_active$text, parent_private$text))
}

#' Resolve an R6 class from box::use imports (module or package)
#' @keywords internal
resolve_r6_from_imports <- function(class_name, module_alias, full_xml, source_file,
                                     visited = character()) {
  if (!is.null(module_alias)) {
    return(resolve_r6_from_module_alias(
      class_name, module_alias, full_xml, source_file, visited
    ))
  }

  # inherit = ClassName — check if it was imported from a module or package
  components <- resolve_r6_from_module_function_import(
    class_name, full_xml, source_file, visited
  )
  if (!is.null(components)) {
    return(components)
  }

  # Check package imports: box::use(pkg[ClassName])
  return(resolve_r6_from_package_import(class_name, full_xml))
}

#' Resolve R6 class from a module alias (inherit = alias$Class)
#' @keywords internal
resolve_r6_from_module_alias <- function(class_name, module_alias, full_xml, source_file,
                                          visited = character()) {
  # Find box::use import that defines this alias
  # The alias appears as SYMBOL_SUB in box::use(alias = ./path)
  xpath_alias <- glue::glue(
    "//SYMBOL_SUB[text() = '{module_alias}']/following-sibling::EQ_SUB[1]/following-sibling::expr[1]"
  )
  path_expr <- xml2::xml_find_all(full_xml, xpath_alias)

  if (length(path_expr) == 0) {
    return(character())
  }

  # Check it's inside a box::use call and contains a path (OP-SLASH)
  has_slash <- length(xml2::xml_find_all(path_expr[[1]], ".//OP-SLASH")) > 0
  if (!has_slash) {
    return(character())
  }

  # Reconstruct the module path from the SYMBOL nodes
  symbols <- xml2::xml_find_all(path_expr[[1]], ".//SYMBOL")
  module_path <- paste(xml2::xml_text(symbols), collapse = "/")

  return(resolve_r6_from_module_file(class_name, module_path, source_file, visited))
}

#' Resolve R6 class from a function import (box::use(./path\[ClassName\]))
#' @keywords internal
resolve_r6_from_module_function_import <- function(class_name, full_xml, source_file,
                                                    visited = character()) {
  # Find box::use module imports that have [ClassName] attached
  # Pattern: box::use(./path/to/mod[ClassName, ...])
  # In XML: the class_name appears as SYMBOL inside OP-LEFT-BRACKET...OP-RIGHT-BRACKET
  # which is inside an expr that also contains OP-SLASH (module path)
  mod_base <- box_module_base_path()

  # Find SYMBOL nodes matching class_name inside bracket imports of module paths
  xpath <- glue::glue(
    "{mod_base}//SYMBOL[text() = '{class_name}']"
  )
  nodes <- xml2::xml_find_all(full_xml, xpath)

  if (length(nodes) == 0) {
    # Also check SYMBOL_SUB (named imports like alias = ClassName)
    xpath_sub <- glue::glue(
      "{mod_base}//SYMBOL_SUB[text() = '{class_name}']"
    )
    nodes <- xml2::xml_find_all(full_xml, xpath_sub)
  }

  if (length(nodes) == 0) {
    return(NULL)
  }

  # Walk up from the class_name node to find the module path (contains OP-SLASH)
  for (node in nodes) {
    ancestor <- xml2::xml_parent(node)
    for (i in 1:6) {
      slashes <- xml2::xml_find_all(ancestor, ".//OP-SLASH")
      if (length(slashes) > 0) {
        # Found the import expr containing the path — get path symbols before the bracket
        # Path symbols are the SYMBOL nodes that are part of the path, not inside brackets
        all_symbols <- xml2::xml_find_all(ancestor, "./expr/SYMBOL | ./OP-SLASH/preceding-sibling::expr/SYMBOL | ./OP-SLASH/following-sibling::expr/SYMBOL | ./OP-SLASH/following-sibling::expr/expr/SYMBOL[not(ancestor::OP-LEFT-BRACKET)]")
        # Filter: only symbols before the bracket
        path_parts <- character()
        for (sym in all_symbols) {
          sym_text <- xml2::xml_text(sym)
          if (sym_text == class_name) break
          path_parts <- c(path_parts, sym_text)
        }
        if (length(path_parts) > 0) {
          module_path <- paste(path_parts, collapse = "/")
          components <- resolve_r6_from_module_file(class_name, module_path, source_file, visited)
          if (!is.null(components)) {
            return(components)
          }
        }
        break
      }
      ancestor <- xml2::xml_parent(ancestor)
    }
  }

  return(NULL)
}

#' Resolve R6 class from a package import (box::use(pkg\[ClassName\]))
#' @keywords internal
resolve_r6_from_package_import <- function(class_name, full_xml) {
  # Find package imports that include class_name
  xpath <- glue::glue(
    "{box_package_base_path()}//SYMBOL[text() = '{class_name}']"
  )
  nodes <- xml2::xml_find_all(full_xml, xpath)

  if (length(nodes) == 0) {
    return(character())
  }

  # Get the package name
  for (node in nodes) {
    pkg_name <- tryCatch({
      # Walk up to find the package name in the import
      import_expr <- xml2::xml_parent(xml2::xml_parent(node))
      pkg_nodes <- xml2::xml_find_all(import_expr, ".//expr/SYMBOL")
      if (length(pkg_nodes) > 0) {
        xml2::xml_text(pkg_nodes[[1]])
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(pkg_name)) {
      components <- resolve_r6_from_package(class_name, pkg_name)
      if (!is.null(components)) {
        return(components)
      }
    }
  }

  return(character())
}

#' Load an R6 class from a module source file and extract components
#'
#' Parses the module file, finds the R6Class definition, extracts its
#' components, and recursively follows its own inherit chain.
#'
#' @param class_name Name of the R6 class to find.
#' @param module_path Module path from the box::use import (e.g., "./base_mod").
#' @param source_file Path of the file being linted (for resolving relative paths).
#' @param visited Character vector of already-visited class names (cycle guard).
#' @keywords internal
resolve_r6_from_module_file <- function(class_name, module_path, source_file,
                                         visited = character()) {
  if (is.null(source_file)) {
    return(NULL)
  }

  # Resolve module path relative to source file's directory
  working_dir <- fs::path_dir(source_file)
  mod_path <- gsub("^\\./", "", module_path)
  mod_file <- file.path(working_dir, paste0(mod_path, ".R"))

  if (!file.exists(mod_file)) {
    return(NULL)
  }

  tryCatch({
    parsed <- parse(mod_file, keep.source = TRUE)
    xml_data <- xmlparsedata::xml_parse_data(parsed)
    mod_xml <- xml2::read_xml(xml_data)

    # Find the R6Class assigned to class_name
    xpath <- glue::glue(
      "//expr[LEFT_ASSIGN and expr/SYMBOL[text() = '{class_name}']]"
    )
    assign_nodes <- xml2::xml_find_all(mod_xml, xpath)
    r6_nodes <- xml2::xml_find_all(
      assign_nodes,
      ".//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'R6Class']]"
    )

    if (length(r6_nodes) == 0) {
      return(NULL)
    }

    public <- get_r6_components(r6_nodes[[1]], "public")
    active <- get_r6_components(r6_nodes[[1]], "active")
    private <- get_r6_components(r6_nodes[[1]], "private")
    own_components <- c(public$text, active$text, private$text)

    # Recurse: follow this class's own inherit chain
    parent_components <- get_r6_inherited_components(
      r6_nodes[[1]],
      mod_xml,
      source_file = mod_file,
      visited = c(visited, class_name)
    )

    return(c(own_components, parent_components))
  }, error = function(e) {
    return(NULL)
  })
}

#' Load an R6 class from an installed package and extract components
#' @keywords internal
resolve_r6_from_package <- function(class_name, pkg_name) {
  tryCatch({
    cls <- get(class_name, envir = asNamespace(pkg_name))
    if (!inherits(cls, "R6ClassGenerator")) {
      return(NULL)
    }
    public <- c(
      paste("self", names(cls$public_fields), sep = "$"),
      paste("self", names(cls$public_methods), sep = "$")
    )
    private <- c(
      paste("private", names(cls$private_fields), sep = "$"),
      paste("private", names(cls$private_methods), sep = "$")
    )
    active <- paste("self", names(cls$active), sep = "$")
    return(c(public, private, active))
  }, error = function(e) {
    return(NULL)
  })
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
