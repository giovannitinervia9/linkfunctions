#' The Rhobit (Fisher's z) Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Rhobit transformation, also known as Fisher's z-transformation.
#' This link function maps the open interval \code{(-1, 1)} to the real line \code{(-Inf, Inf)}.
#' It is primarily used for modeling correlation coefficients or other bounded parameters that range between -1 and 1.
#'
#' @details
#' The Rhobit link is defined using the inverse hyperbolic tangent function:
#' \eqn{\eta = \text{arctanh}(\theta) = \frac{1}{2} \log\left(\frac{1 + \theta}{1 - \theta}\right)}.
#'
#' The inverse link is the hyperbolic tangent function:
#' \eqn{\theta = \tanh(\eta) = \frac{\exp(2\eta) - 1}{\exp(2\eta) + 1}}.
#'
#'
#' The domain of \eqn{\theta} is \code{(-1, 1)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{logit_link}}
#'
#' @export
#'
#' @examples
#' l <- rhobit_link()
#'
#' # Link function: g(0) should be 0 (atanh(0) = 0)
#' l$linkfun(0)
#'
#' # Inverse link: g^-1(Inf) approaches 1
#' l$linkinv(10)
#'
#' # Derivative checks
#' l$theta_eta(0) # 1 - 0^2 = 1
rhobit_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "rhobit"
  o$link_bounds <- c(-1, 1)

  o$linkfun <- function(theta) {
    atanh(theta)
  }
  o$linkinv <- function(eta) {
    tanh(eta)
  }

  o$theta_eta <- function(eta) {
    t <- tanh(eta)
    1 - t^2
  }
  o$theta2_eta2 <- function(eta) {
    t <- tanh(eta)
    -2 * t * (1 - t^2)
  }

  o$eta_theta <- function(theta) {
    1 / (1 - theta^2)
  }
  o$eta2_theta2 <- function(theta) {
    2 * theta / (1 - theta^2)^2
  }

  o$link_params <- NULL

  o
}
