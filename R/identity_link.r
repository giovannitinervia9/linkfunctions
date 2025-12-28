#' The Identity Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Identity transformation.
#' This is the canonical link function for mean parameter of the Normal (Gaussian) distribution.
#'
#' @details
#' The Identity link is defined simply as \eqn{\eta = \theta}.
#' Consequently, the inverse link is also \eqn{\theta = \eta}.
#'
#' All first derivatives are constant (equal to 1), and all second derivatives are zero.
#'
#' The domain of \eqn{\theta} is unbounded: \code{(-Inf, Inf)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{logit_link}}
#'
#' @export
identity_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "identity"

  o$link_bounds <- c(-Inf, Inf)

  o$linkfun <- function(theta) {
    theta
  }
  o$linkinv <- function(eta) {
    eta
  }

  o$theta_eta <- function(eta) {
    rep(1, length(eta))
  }
  o$theta2_eta2 <- function(eta) {
    rep(0, length(eta))
  }

  o$eta_theta <- function(theta) {
    rep(1, length(theta))
  }
  o$eta2_theta2 <- function(theta) {
    rep(0, length(theta))
  }

  o$link_params <- NULL

  o
}
