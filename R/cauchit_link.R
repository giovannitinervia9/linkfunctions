#' The Cauchit Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Cauchit transformation.
#' This link function is based on the quantile function of the standard Cauchy distribution.
#'
#' @details
#' The Cauchit link is defined as \eqn{\eta = \tan(\pi(\theta - 0.5))}, corresponding to
#' \code{qcauchy(theta)}.
#'
#' Unlike the Logit or Probit links, the Cauchit link has heavier tails. This makes it
#' particularly useful for modeling binary data where the probability approaches 0 or 1
#' more slowly, or when the data contains outliers that might influence the fit of light-tailed links.
#'
#' The domain of \eqn{\theta} is \code{(0, 1)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{logit_link}}, \code{\link[stats]{qcauchy}}
#' @importFrom stats qcauchy pcauchy dcauchy
#' @export
cauchit_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "cauchit"
  o$link_bounds <- c(0, 1)

  o$linkfun <- function(theta) {
    qcauchy(theta)
  }
  o$linkinv <- function(eta) {
    pcauchy(eta)
  }

  o$theta_eta <- function(eta) {
    dcauchy(eta)
  }
  o$theta2_eta2 <- function(eta) {
    -2 * eta / (pi * (1 + eta^2)^2)
  }

  o$eta_theta <- function(theta) {
    1 / dcauchy(qcauchy(theta))
  }
  o$eta2_theta2 <- function(theta) {
    eta <- qcauchy(theta)
    d_pdf_d_eta <- -2 * eta / (pi * (1 + eta^2)^2)
    -d_pdf_d_eta / (dcauchy(eta)^3)
  }

  o$link_params <- NULL

  o
}
