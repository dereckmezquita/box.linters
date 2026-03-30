test_that("r6_usage_linter allows R6Class and R6$R6Class function calls.", {
  linter <- r6_usage_linter()

  good_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      initialize = function(...) {
        # do something
      }
    )
  )
  "

  good_r6_class_2 <- "box::use(
    R6,
  )

  newClass <- R6$R6Class('newClass',
    public = list(
      initialize = function(...) {
        # do something
      }
    )
  )
  "

  lintr::expect_lint(good_r6_class_1, NULL, linter)
  lintr::expect_lint(good_r6_class_2, NULL, linter)
})


test_that("r6_usage_linter skips valid R6 classes.", {
  linter <- r6_usage_linter()

  good_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  lintr::expect_lint(good_r6_class_1, NULL, linter)
})

test_that("r6_usage_linter handles more than one good R6 classes in the same file.", {
  linter <- r6_usage_linter()

  good_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  firstClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )

  secondClass <- R6Class('secondClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  lintr::expect_lint(good_r6_class_1, NULL, linter)
})

test_that("r6_usage_linter skips non-R6 class definitions in the same file.", {
  linter <- r6_usage_linter()

  good_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )

  some_function <- function() {
    non_existing_function()
  }

  fs$path_file('path/to/file')
  "

  lintr::expect_lint(good_r6_class_1, NULL, linter)
})

test_that("r6_usage_linter blocks unused private objects (properties and methods).", {
  linter <- r6_usage_linter()
  lint_message <- rex::rex("Private object not used.")

  # property
  bad_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {

      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  # emthod
  bad_r6_class_2 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {

      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  lintr::expect_lint(bad_r6_class_1, list(message = lint_message), linter)
  lintr::expect_lint(bad_r6_class_2, list(message = lint_message), linter)
})

test_that("r6_usage_linter blocks unused private objects in second class", {
  linter <- r6_usage_linter()
  lint_message <- rex::rex("Private object not used.")

  # property
  bad_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  firstClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )

  secondClass <- R6Class('secondClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {

      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )"

  # emthod
  bad_r6_class_2 <- "box::use(
    R6[R6Class],
  )

  firstClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {

      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )

  secondClass <- R6Class('secondClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )"

  lintr::expect_lint(bad_r6_class_1, list(message = lint_message), linter)
  lintr::expect_lint(bad_r6_class_2, list(message = lint_message), linter)
})


test_that("r6_usage_linter blocks internal calls to invalid public objects", {
  linter <- r6_usage_linter()
  lint_message <- rex::rex("Internal object call not found.")

  # property
  bad_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$no_such_property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  # method
  bad_r6_class_2 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$no_such_method(value)
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  lintr::expect_lint(bad_r6_class_1, list(message = lint_message), linter)
  lintr::expect_lint(bad_r6_class_2, list(message = lint_message), linter)
})

test_that("r6_usage_linter blocks internal calls to invalid active objects", {
  linter <- r6_usage_linter()
  lint_message <- rex::rex("Internal object call not found.")

  bad_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
        self$no_such_active_binding <- value
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  lintr::expect_lint(bad_r6_class_1, list(message = lint_message), linter)
})

test_that("r6_usage_linter blocks internal calls to invalid private objects", {
  linter <- r6_usage_linter()
  lint_message <- rex::rex("Internal object call not found.")

  # property
  bad_r6_class_1 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
        private$no_such_property <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  # method
  bad_r6_class_2 <- "box::use(
    R6[R6Class],
  )

  newClass <- R6Class('newClass',
    public = list(
      property = NULL,
      initialize = function(value) {
        self$property <- value
      },
      external_method = function(value) {
        private$internal_method(value)
        private$no_such_method(value)
      }
    ),
    active = list(
      binding = function(value) {
        private$secret <- value
      }
    ),
    private = list(
      secret = NULL,
      internal_method = function(value) {
        private$another_method()
      },
      another_method = function() {
        # do something
      }
    )
  )
  "

  lintr::expect_lint(bad_r6_class_1, list(message = lint_message), linter)
  lintr::expect_lint(bad_r6_class_2, list(message = lint_message), linter)
})

test_that("r6_usage_linter allows inherited methods from parent R6 class", {
  linter <- r6_usage_linter()

  good_r6_inherit <- "box::use(
    R6[R6Class],
  )

  Parent <- R6Class('Parent',
    public = list(
      log_msg = function() { },
      parent_attr = NULL
    )
  )

  Child <- R6Class('Child',
    inherit = Parent,
    public = list(
      run = function() {
        self$log_msg()
        self$parent_attr
      }
    )
  )
  "

  lintr::expect_lint(good_r6_inherit, NULL, linter)
})

test_that("r6_usage_linter allows methods inherited from grandparent R6 class", {
  linter <- r6_usage_linter()

  good_r6_grandparent <- "box::use(
    R6[R6Class],
  )

  GrandParent <- R6Class('GrandParent',
    public = list(
      ancient_method = function() { }
    )
  )

  Parent <- R6Class('Parent',
    inherit = GrandParent,
    public = list(
      log_msg = function() { }
    )
  )

  Child <- R6Class('Child',
    inherit = Parent,
    public = list(
      run = function() {
        self$log_msg()
        self$ancient_method()
      }
    )
  )
  "

  lintr::expect_lint(good_r6_grandparent, NULL, linter)
})

test_that("r6_usage_linter blocks invalid calls in child class even with inheritance", {
  linter <- r6_usage_linter()
  lint_message <- rex::rex("Internal object call not found.")

  bad_r6_inherit <- "box::use(
    R6[R6Class],
  )

  Parent <- R6Class('Parent',
    public = list(
      log_msg = function() { }
    )
  )

  Child <- R6Class('Child',
    inherit = Parent,
    public = list(
      run = function() {
        self$nonexistent()
      }
    )
  )
  "

  lintr::expect_lint(bad_r6_inherit, list(message = lint_message), linter)
})

test_that("r6_usage_linter allows inherited methods from parent in another module (alias$Class)", {
  linter <- r6_usage_linter()

  withr::with_options(list(box.path = NULL), {
    test_file <- file.path(getwd(), "mod", "local_mod", "child_alias.R")
    results <- lintr::lint(test_file, linters = list(linter))
    expect_length(results, 0)
  })
})

test_that("r6_usage_linter allows inherited methods from parent in another module (direct import)", {
  linter <- r6_usage_linter()

  withr::with_options(list(box.path = NULL), {
    test_file <- file.path(getwd(), "mod", "local_mod", "child_direct.R")
    results <- lintr::lint(test_file, linters = list(linter))
    expect_length(results, 0)
  })
})

test_that("r6_usage_linter allows cross-file chain inheritance (A -> B -> C across 3 files)", {
  linter <- r6_usage_linter()

  # child_chain.R inherits from parent_cross.R which inherits from grandparent.R
  # self$ancient_method() comes from the grandparent two files away
  withr::with_options(list(box.path = NULL), {
    test_file <- file.path(getwd(), "mod", "local_mod", "child_chain.R")
    results <- lintr::lint(test_file, linters = list(linter))
    expect_length(results, 0)
  })
})

test_that("r6_usage_linter allows inherited active bindings and private members", {
  linter <- r6_usage_linter()

  good_r6 <- "box::use(
    R6[R6Class],
  )

  Parent <- R6Class('Parent',
    public = list(
      pub_method = function() {
        private$helper()
      }
    ),
    active = list(
      status = function() { 'ok' }
    ),
    private = list(
      helper = function() { }
    )
  )

  Child <- R6Class('Child',
    inherit = Parent,
    public = list(
      run = function() {
        self$pub_method()
        self$status
        private$helper()
      }
    )
  )
  "

  lintr::expect_lint(good_r6, NULL, linter)
})

test_that("r6_usage_linter handles parent class not found (no crash)", {
  linter <- r6_usage_linter()

  # UnknownParent is not defined anywhere -- linter should not crash,
  # just can't validate inherited calls (no lint for self$whatever)
  code <- "box::use(
    R6[R6Class],
  )

  Child <- R6Class('Child',
    inherit = UnknownParent,
    public = list(
      run = function() {
        self$whatever()
      }
    )
  )
  "

  # Should produce 1 lint (self$whatever not found) but NOT crash
  lint_message <- rex::rex("Internal object call not found.")
  lintr::expect_lint(code, list(message = lint_message), linter)
})

test_that("r6_usage_linter handles circular inheritance without infinite loop", {
  linter <- r6_usage_linter()

  # Circular: A inherits B, B inherits A
  # This can't actually run in R6, but the linter shouldn't crash
  code <- "box::use(
    R6[R6Class],
  )

  ClassA <- R6Class('ClassA',
    inherit = ClassB,
    public = list(
      method_a = function() { }
    )
  )

  ClassB <- R6Class('ClassB',
    inherit = ClassA,
    public = list(
      method_b = function() { }
    )
  )
  "

  # Should not hang or crash -- just lint normally
  results <- lintr::lint(text = code, linters = list(linter))
  expect_true(is.list(results))
})
