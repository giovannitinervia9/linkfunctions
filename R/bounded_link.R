#' The General Bounded Link Function
#'
#' @description
#' Creates an object of class \code{"link"} that maps a finite interval \code{(lwr, upr)} to the whole real line.
#' This is achieved by scaling and shifting the standard Logit transformation. It is ideal for parameters
#' constrained within a specific range, such as percentages (0-100) or physical bounds.
#'
#' @param lwr Numeric. The lower bound of the interval.
#' @param upr Numeric. The upper bound of the interval. Must be greater than \code{lwr}.
#'
#' @details
#' The Bounded link transforms \eqn{\theta} by first normalizing it to \code{(0, 1)} via \eqn{p = \frac{\theta - \text{lwr}}{\text{upr} - \text{lwr}}},
#' and then applying the logit function.
#'
#' Formally:
#' \eqn{\eta = \log\left(\frac{\theta - \text{lwr}}{\text{upr} - \theta}\right)}
#'
#' Inverse:
#' \eqn{\theta = \text{lwr} + (\text{upr} - \text{lwr}) \cdot \text{logit}^{-1}(\eta)}
#'
#' The domain of \eqn{\theta} is \code{(lwr, upr)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}).
#'
#' @seealso \code{\link{link-class}}, \code{\link{logit_link}}, \code{\link{lower_bounded_link}}
#' @importFrom stats qlogis plogis
#' @export
bounded_link <- function(lwr = 0, upr = 1) {
  if (lwr >= upr) {
    stop("Lower bound 'lwr' must be strictly less than upper bound 'upr'.")
  }

  o <- list()
  class(o) <- "link"
  o$link_name <- paste0("bounded(lwr=", lwr, ", upr=", upr, ")")

  o$link_bounds <- c(lwr, upr)

  range_width <- upr - lwr

  o$linkfun <- function(theta) {
    p <- (theta - lwr) / range_width
    qlogis(p)
  }

  o$linkinv <- function(eta) {
    p <- plogis(eta)
    lwr + p * range_width
  }

  o$theta_eta <- function(eta) {
    p <- plogis(eta)
    range_width * p * (1 - p)
  }

  o$theta2_eta2 <- function(eta) {
    p <- plogis(eta)
    range_width * p * (1 - p) * (1 - 2 * p)
  }

  o$eta_theta <- function(theta) {
    range_width / ((theta - lwr) * (upr - theta))
  }

  o$eta2_theta2 <- function(theta) {
    numerator <- range_width * (upr + lwr - 2 * theta)
    denominator <- ((theta - lwr) * (upr - theta))^2
    -numerator / denominator
  }

  o$link_params <- list(lwr = lwr, upr = upr)
  o
}
