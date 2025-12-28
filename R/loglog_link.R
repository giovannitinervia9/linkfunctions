#' The Log-Log Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Log-Log transformation.
#' This link is the symmetric opposite of the `cloglog` link and is often used in survival analysis
#' (Gompertz models) or for modeling binary data with specific asymmetry requirements.
#'
#' @details
#' The Log-Log link is defined as \eqn{\eta = -\log(-\log(\theta))}.
#' The inverse link is \eqn{\theta = \exp(-\exp(-\eta))}.
#'
#' **Asymmetry:** Unlike the symmetric Logit link, the Log-Log link is asymmetric.
#' It is suitable for modeling events where the probability approaches 0 slowly but approaches 1 sharply.
#' This is the mathematical reverse of the \code{\link{cloglog_link}}.
#'
#' The domain of \eqn{\theta} is \code{(0, 1)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{cloglog_link}}, \code{\link{logit_link}}
#'
#' @export
loglog_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "loglog"
  o$link_bounds <- c(0, 1)

  o$linkfun <- function(theta) {
    -log(-log(theta))
  }
  o$linkinv <- function(eta) {
    exp(-exp(-eta))
  }

  o$theta_eta <- function(eta) {
    exp(-eta - exp(-eta))
  }
  o$theta2_eta2 <- function(eta) {
    exp(-eta - exp(-eta)) * (exp(-eta) - 1)
  }

  o$eta_theta <- function(theta) {
    -1 / (theta * log(theta))
  }
  o$eta2_theta2 <- function(theta) {
    (1 + log(theta)) / (theta^2 * (log(theta))^2)
  }
  o$link_params <- NULL

  o
}
