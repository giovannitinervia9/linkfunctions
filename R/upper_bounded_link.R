#' The Upper Bounded Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing a logarithmic transformation for parameters bounded from above.
#' This is useful for modeling parameters that must remain strictly below a certain threshold \code{upr}.
#'
#' @param upr A numeric value specifying the upper bound.
#'
#' @details
#' The Upper Bounded link is defined as \eqn{\eta = \log(\text{upr} - \theta)}.
#' The inverse link is \eqn{\theta = \text{upr} - \exp(\eta)}.
#'
#' Note that as \eqn{\eta \to \infty}, \eqn{\theta \to -\infty}, and as \eqn{\eta \to -\infty}, \eqn{\theta \to \text{upr}}.
#'
#' The domain of \eqn{\theta} is \code{(-Inf, upr)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}).
#'
#' @seealso \code{\link{link-class}}, \code{\link{lower_bounded_link}}
#'
#' @export
upper_bounded_link <- function(upr = 0) {
  o <- list()
  class(o) <- "link"
  o$link_name <- paste0("upper_bounded(upr=", upr, ")")

  o$link_bounds <- c(-Inf, upr)

  o$linkfun <- function(theta) {
    log(upr - theta)
  }
  o$linkinv <- function(eta) {
    upr - exp(eta)
  }

  o$theta_eta <- function(eta) {
    -exp(eta)
  }
  o$theta2_eta2 <- function(eta) {
    -exp(eta)
  }

  o$eta_theta <- function(theta) {
    -1 / (upr - theta)
  }
  o$eta2_theta2 <- function(theta) {
    -1 / (upr - theta)^2
  }

  o$link_params <- list(upr = upr)
  o
}
