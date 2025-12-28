#' The Power Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the Power transformation family.
#' This function generates a specific link function based on the provided power parameter \code{lambda}.
#' It includes the special case where \code{lambda = 0}, which returns the Log link.
#'
#' @param lambda A numeric value defining the power of the transformation. Defaults to 1.
#'
#' @details
#' The Power link is defined as \eqn{\eta = \theta^\lambda}.
#' The inverse link is \eqn{\theta = \eta^{1/\lambda}}.
#'
#' **Special Case (Box-Cox continuity):**
#' If \code{lambda = 0}, the function automatically returns the \code{\link{log_link}},
#' as \eqn{\lim_{\lambda \to 0} \frac{\theta^\lambda - 1}{\lambda} = \log(\theta)}.
#'
#' Common special cases include:
#' \itemize{
#'   \item \code{lambda = 1}: Identity link.
#'   \item \code{lambda = 0.5}: Square-root link.
#'   \item \code{lambda = -1}: Inverse link.
#'   \item \code{lambda = 0}: Log link.
#' }
#'
#' The domain of \eqn{\theta} is \code{(0, Inf)}.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions, their exact analytical derivatives, and the
#' \code{link_params} field storing the value of \code{lambda}.
#'
#' @seealso \code{\link{link-class}}, \code{\link{log_link}}, \code{\link{identity_link}}
#'
#' @export
power_link <- function(lambda = 1) {
  if (lambda == 0) {
    o <- log_link()
    o$link_name <- "power(lambda=0)"
    return(o)
  }

  o <- list()
  class(o) <- "link"
  o$link_name <- paste0("power(lambda=", round(lambda, 5), ")")

  o$link_bounds <- c(0, Inf)

  o$linkfun <- function(theta) {
    theta^lambda
  }
  o$linkinv <- function(eta) {
    eta^(1 / lambda)
  }

  o$theta_eta <- function(eta) {
    (1 / lambda) * eta^(1 / lambda - 1)
  }
  o$theta2_eta2 <- function(eta) {
    k <- 1 / lambda
    k * (k - 1) * eta^(k - 2)
  }

  o$eta_theta <- function(theta) {
    lambda * theta^(lambda - 1)
  }
  o$eta2_theta2 <- function(theta) {
    lambda * (lambda - 1) * theta^(lambda - 2)
  }
  o$link_params <- c(lambda = lambda)

  o
}
