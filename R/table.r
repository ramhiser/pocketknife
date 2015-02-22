#' Contingency table including NA values
#'
#' This function is a simple wrapper around \code{\link[base]{table}} to build
#' a contingency table including \code{NA} values.
#'
#' @export
#' @param ... one or more objects passed to \code{\link[base]{table}} to build
#' a contingency table
#' @return a \code{table} object containing the contingency table
table_na <- function(...) {
  table(..., useNA="ifany")
}
