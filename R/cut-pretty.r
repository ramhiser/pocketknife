#' Cuts a vector into factors with pretty levels
#'
#' @importFrom itertools2 ipairwise
#' @export
#' @param x numeric vectory
#' @param breaks numeric vector of two ore more unique cut points
#' @param ... arguments passed to \code{\link[base]{cut}}
#' @return A \code{\link{factor}} is returned
#'
#' @examples
#' set.seed(42)
#' x <- runif(n=50, 0, 50)
#' cut_pretty(x, breaks=pretty(x))
cut_pretty <- function(x, breaks, collapse=" to ", ...) {
  it_breaks <- itertools2::ipairwise(breaks)
  breaks_pretty <- sapply(it_breaks, paste, collapse=collapse)
  
  cut(x, breaks=breaks, labels=breaks_pretty, ...)
}
