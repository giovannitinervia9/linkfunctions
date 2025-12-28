#' The Lower Bounded Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing a logarithmic transformation shifted by a lower bound.
#' This allows modeling parameters that must be strictly greater than a specific value \code{lwr} (e.g., degrees of freedom > 2).
#'
#' @param lwr A numeric value specifying the lower bound. Defaults to 0 (which is equivalent to the \code{\link{log_link}}).
#'
#' @details
#' The Lower Bounded link is defined as \eqn{\eta = \log(\theta - \text{lwr})}.
#' The inverse link is \eqn{\theta = \exp(\eta) + \text{lwr}}.
#'
#' The domain of \eqn{\theta} is \code{(lwr, Inf)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}).
#'
#' @seealso \code{\link{link-class}}, \code{\link{log_link}}, \code{\link{upper_bounded_link}}
#'
#' @export
lower_bounded_link <- function(lwr = 0) {
  o <- list()
  class(o) <- "link"
  o$link_name <- paste0("lower_bounded(lwr=", lwr, ")")

  o$link_bounds <- c(lwr, Inf)

  o$linkfun <- function(theta) {
    log(theta - lwr)
  }
  o$linkinv <- function(eta) {
    lwr + exp(eta)
  }

  o$theta_eta <- function(eta) {
    exp(eta)
  }
  o$theta2_eta2 <- function(eta) {
    exp(eta)
  }

  o$eta_theta <- function(theta) {
    1 / (theta - lwr)
  }
  o$eta2_theta2 <- function(theta) {
    -1 / (theta - lwr)^2
  }

  o$link_params <- list(lwr = lwr)
  o
}
