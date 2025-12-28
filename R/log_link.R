#' The Logarithmic Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the natural logarithm transformation.
#' This is the canonical link function for mean of the Poisson distribution and is widely used for modeling count data
#' or non-negative continuous data with multiplicative effects.
#'
#' @details
#' The Log link is defined as \eqn{\eta = \log(\theta)}.
#' The inverse link is the exponential function \eqn{\theta = \exp(\eta)}.
#'
#' A mathematical property of this link is that the inverse function is its own derivative.
#' Therefore, \eqn{\theta}, \eqn{d\theta/d\eta}, and \eqn{d^2\theta/d\eta^2} are all equal to \eqn{\exp(\eta)}.
#'
#' The domain of \eqn{\theta} is \code{(0, Inf)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{inverse_link}}
#'
#' @export
log_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "log"
  o$link_bounds <- c(0, Inf)

  o$linkfun <- function(theta) {
    log(theta)
  }
  o$linkinv <- o$theta_eta <- o$theta2_eta2 <- function(eta) {
    pmax(exp(eta), .Machine$double.eps)
  }

  o$eta_theta <- function(theta) {
    1 / theta
  }
  o$eta2_theta2 <- function(theta) {
    -1 / theta^2
  }

  o$link_params <- NULL

  o
}
