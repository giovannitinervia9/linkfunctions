#' The Probit Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Probit transformation.
#' This link function relies on the cumulative distribution function (CDF) of the standard Normal distribution.
#' It is widely used in Generalized Linear Models (GLMs) for binary data, often justified by a latent normal variable interpretation.
#'
#' @details
#' The Probit link is defined as \eqn{\eta = \Phi^{-1}(\theta)}, where \eqn{\Phi^{-1}} is the quantile function of the standard normal distribution (\code{qnorm}).
#' The inverse link is \eqn{\theta = \Phi(\eta)}, the standard normal CDF (\code{pnorm}).
#'
#' Like the \code{\link{logit_link}}, the Probit is symmetric around \eqn{\theta = 0.5} (where \eqn{\eta = 0}).
#' However, the tails of the Normal distribution approach 0 and 1 faster than the Logistic distribution.
#'
#' The domain of \eqn{\theta} is \code{(0, 1)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{logit_link}}, \code{\link{cauchit_link}}, \code{\link[stats]{Normal}}
#' @importFrom stats qnorm pnorm dnorm
#' @export
probit_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "probit"
  o$link_bounds <- c(0, 1)

  o$linkfun <- function(theta) {
    qnorm(theta)
  }
  o$linkinv <- function(eta) {
    pnorm(eta)
  }

  o$theta_eta <- function(eta) {
    dnorm(eta)
  }
  o$theta2_eta2 <- function(eta) {
    -eta * dnorm(eta)
  }

  o$eta_theta <- function(theta) {
    1 / dnorm(qnorm(theta))
  }
  o$eta2_theta2 <- function(theta) {
    eta <- qnorm(theta)
    eta / (dnorm(eta)^2)
  }
  o$link_params <- NULL

  o
}
