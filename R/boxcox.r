#' Helper function to calculate optimal Box-Cox power transformation
#'
#' A Box-Cox (BC) transformation is often used to transform data to achieve a
#' symmetric, unimodal distribution (typically, of the response). The BC
#' transformation can be estimated via \code{\link[car]{boxCox}}. The code to
#' find the optimal value is a bit cumbersome. In this function, we streamline
#' the process.
#'
#' The BC transformation requires a positive response. In this case, the
#' Yeo-Johnson (YJ) transformation generalizes the BC transformation to allow
#' all real values.
#'
#' @importFrom car boxCox
#' @export
#' @param x numeric data vector
#' @param plot logical. Plot the BC's objective function? Default: no.
#' @param family transformation family to use: Box-Cox or Yeo-Johnson. The
#' options match those in \code{\link[car]{boxCox}}
#' @param ... additional arguments passed to \code{\link[car]{boxCox}}
#' @return list with the transformed data in \code{x} and the optimal BC value
#' in \code{lambda}.
boxcox_transform <- function(x, plot=FALSE, family=c("bcPower", "yjPower"), ...) {
  family <- match.arg(family)

  df <- data.frame(x=x)
  bc_out <- car::boxCox(x ~ 1, data=df, family=family, plotit=plot, interp=TRUE, ...)
  lambda <- with(bc_out, x[which.max(y)])

  if (family == "bcPower") {
    x <- bcPower(x, lambda=lambda)
  } else if (family == "yjPower") {
    x <- yjPower(x, lambda=lambda)
  }
  list(x=x, lambda=lambda)
}

#' Inverse Box-Cox Transformation
#'
#' Often, a data transformation is necessary to achieve a symmetric, unimodal
#' distribution. A common approach is the Box-Cox transformation, which can be
#' performed by \code{\link[car]{boxCox}}. While the Box-Cox transformation is
#' given in the \code{\link{car}} package, the inverse transformation is not.
#' This function takes care of that.
#'
#' @export
#' @param y numeric vector
#' @param lambda Box-Cox transformation parameter
#' @return transformed numeric vector
inv_boxcox <- function(y, lambda=0) {
  if (lambda == 0) {
    exp(y)
  } else {
    (lambda * y + 1)^(1 / lambda)
  }
}
