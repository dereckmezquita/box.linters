box::use(./helper)

test_fn <- function() {
  return(helper$nonexistent(1, 2))
}
