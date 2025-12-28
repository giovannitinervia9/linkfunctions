#' The Softplus Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Softplus transformation.
#' The Softplus function is a smooth approximation of the rectifier (ReLU) and ensures the parameter \eqn{\theta} remains positive.
#' Unlike the Log link, which is exponential, the Softplus link becomes linear for large values of the linear predictor.
#'
#' @param a A numeric value specifying the scaling parameter (smoothness/steepness). Must be strictly positive. Defaults to 1.
#'
#' @details
#' The Softplus link describes the relationship where the response parameter \eqn{\theta} is the Softplus of the linear predictor \eqn{\eta}.
#'
#' Mathematically:
#' \itemize{
#'   \item Inverse Link (Softplus): \eqn{\theta = \frac{1}{a} \log(1 + \exp(a \eta))}
#'   \item Link Function: \eqn{\eta = \frac{1}{a} \log(\exp(a \theta) - 1)}
#' }
#'
#' **Behavior:**
#' For large negative \eqn{\eta}, \eqn{\theta \approx 0}.
#' For large positive \eqn{\eta}, \eqn{\theta \approx \eta} (linear behavior), whereas a Log link would imply \eqn{\theta = \exp(\eta)} (exponential behavior).
#'
#' **Numerical Stability:**
#' The inverse link implementation uses the "Log-Sum-Exp" trick (\code{log1pexp}) via conditional logic to ensure numerical stability for large positive values of \eqn{\eta}, avoiding overflow.
#'
#' The domain of \eqn{\theta} is \code{(0, Inf)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{log_link}}, \code{\link{identity_link}}
#' @importFrom stats plogis
#' @export
softplus_link <- function(a = 1) {
  if (a <= 0) {
    stop("Scale parameter 'a' must be greater than 0")
  }

  o <- list()
  class(o) <- "link"
  o$link_name <- paste0("softplus(a=", round(a, 5), ")")

  o$link_bounds <- c(0, Inf)

  o$linkfun <- function(theta) {
    log(expm1(a * theta)) / a
  }
  o$linkinv <- function(eta) {
    pmax(0, eta) + log(1 + exp(-abs(a * eta))) / a
  }

  o$theta_eta <- function(eta) {
    plogis(a * eta)
  }
  o$theta2_eta2 <- function(eta) {
    p <- plogis(a * eta)
    a * p * (1 - p)
  }

  o$eta_theta <- function(theta) {
    exp(a * theta) / expm1(a * theta)
  }
  o$eta2_theta2 <- function(theta) {
    -a * exp(a * theta) / (expm1(a * theta)^2)
  }
  o$link_params <- c(a = a)

  o
}
