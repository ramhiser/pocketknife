#' Retry expression with exponential backoff
#'
#' Attempts the expression in \code{expr} up to the number of tries specified in
#' \code{max_attempts}. Each time a failure results, the functions sleeps for a
#' random amount of time before re-attempting the expression. The upper bound of
#' the backoff increases exponentially after each failure.
#'
#' For details on exponential backoff, see:
#' \url{http://en.wikipedia.org/wiki/Exponential_backoff}
#'
#' @export
#' @param expr an R expression to try.
#' @param silent logical: should the report of error messages be suppressed?
#' @param max_tries the maximum number of times to attempt the expression
#' \code{expr}
#' @param verbose logical: Should detailed messages be reported regarding each
#' attempt? Default: no.
#' @return the value of the expression in \code{expr}. If the final attempt was
#' a failure, the objected returned will be of class try-error".
#'
#' @examples
#' # Silly example that will never succeed.
#' retry(log("a"), verbose=TRUE, max_attempts=5)
retry <- function(expr, silent=FALSE, max_attempts=10, verbose=FALSE) {
  for (attempt_i in seq_len(max_attempts)) {
    results <- try(expr=expr, silent=silent)
    if (class(results) == "try-error") {
      backoff <- runif(n=1, min=0, max=2^attempt_i - 1)
      if (verbose) {
        message("Backing off for ", backoff, " seconds.")
      }
      Sys.sleep(backoff)
    } else {
       if (verbose) {
         message("Succeeded after ", attempt_i, " attempts.")
       }
      break
    }
 }
 results
}
