#' The Inverse Square Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Inverse Square transformation.
#'
#' @details
#' The Inverse Square link is defined as \eqn{\eta = 1 / \theta^2}.
#' The inverse link function is \eqn{\theta = 1 / \sqrt{\eta}}.
#'
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{inverse_link}}, \code{\link[stats]{family}}
#'
#' @export
inverse_sq_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "1/mu^2"
  o$link_bounds <- c(0, Inf)

  o$linkfun <- function(theta) {
    1 / (theta^2)
  }
  o$linkinv <- function(eta) {
    1 / sqrt(eta)
  }

  o$theta_eta <- function(eta) {
    -1 / (2 * eta^(1.5))
  }
  o$theta2_eta2 <- function(eta) {
    0.75 / (eta^2.5)
  }

  o$eta_theta <- function(theta) {
    -2 / (theta^3)
  }
  o$eta2_theta2 <- function(theta) {
    6 / (theta^4)
  }

  o$link_params <- NULL

  o
}
