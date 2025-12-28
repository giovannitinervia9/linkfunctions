#' The Square Root Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Square Root transformation.
#' This is a specific case of the Power link family (with \eqn{\lambda = 0.5}) and is often used
#' for modeling count data (Poisson) as a variance-stabilizing transformation.
#'
#' @details
#' The Square Root link is defined as \eqn{\eta = \sqrt{\theta}}.
#' The inverse link is \eqn{\theta = \eta^2}.
#'
#' Unlike the Log link, this transformation allows \eqn{\theta} to reach 0 exactly.
#' While the inverse function (\eqn{\eta^2}) is mathematically valid for negative \eqn{\eta},
#' in the context of this link function, the linear predictor \eqn{\eta} is typically constrained to be non-negative
#' to preserve a one-to-one mapping with \eqn{\theta}.
#'
#' The domain of \eqn{\theta} is \code{[0, Inf)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{power_link}}, \code{\link{log_link}}
#'
#' @export
sqrt_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "sqrt"

  o$link_bounds <- c(0, Inf)

  o$linkfun <- function(theta) {
    sqrt(theta)
  }
  o$linkinv <- function(eta) {
    eta^2
  }

  o$theta_eta <- function(eta) {
    2 * eta
  }
  o$theta2_eta2 <- function(eta) {
    rep(2, length(eta))
  }

  o$eta_theta <- function(theta) {
    1 / (2 * sqrt(theta))
  }
  o$eta2_theta2 <- function(theta) {
    -1 / (4 * theta^(1.5))
  }

  o$link_params <- NULL

  o
}
