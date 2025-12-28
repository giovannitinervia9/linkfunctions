#' The Complementary Log-Log (ClogLog) Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Complementary Log-Log transformation.
#' This link is asymmetric and is often used for modeling binary data where the probability of the event
#' approaches 1 slowly but approaches 0 sharply (or vice versa depending on formulation).
#'
#' @details
#' The ClogLog link is defined as \eqn{\eta = \log(-\log(1 - \theta))}.
#' The inverse link is \eqn{\theta = 1 - \exp(-\exp(\eta))}.
#'
#' Unlike the symmetric Logit and Probit links, ClogLog is asymmetric. It is closely related to the
#' Gumbel distribution and is frequently used in survival analysis (proportional hazards models)
#' and for modeling rare events.
#'
#' The domain of \eqn{\theta} is \code{(0, 1)}.
#'
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{logit_link}}
#'
#' @export
cloglog_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "cloglog"
  o$link_bounds <- c(0, 1)

  o$linkfun <- function(theta) {
    log(-log(1 - theta))
  }
  o$linkinv <- function(eta) {
    pmax(pmin(1 - exp(-exp(eta)), 1 - .Machine$double.eps), .Machine$double.eps)
  }

  o$theta_eta <- function(eta) {
    eta <- pmin(eta, 700)
    exp(eta) * exp(-exp(eta))
  }
  o$theta2_eta2 <- function(eta) {
    eta <- pmin(eta, 700)
    e_eta <- exp(eta)
    e_eta * exp(-e_eta) * (1 - e_eta)
  }

  o$eta_theta <- function(theta) {
    1 / ((theta - 1) * log(1 - theta))
  }
  o$eta2_theta2 <- function(theta) {
    log_term <- log(1 - theta)
    -(1 + log_term) / ((1 - theta)^2 * log_term^2)
  }

  o$link_params <- NULL

  o
}
