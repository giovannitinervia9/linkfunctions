#' The Inverse (Reciprocal) Link Function
#'
#' @description
#' Creates an object of class \code{"link"} implementing the reciprocal transformation.
#'
#' @details
#' The Inverse link is defined as \eqn{\eta = 1/\theta}.
#' The inverse link function is therefore \eqn{\theta = 1/\eta}.
#'
#' This link is typically used for modeling positive continuous data where the mean is inversely proportional
#' to the linear predictor.
#'
#' The domain of \eqn{\theta} is conventionally \code{(0, Inf)}. Care must be taken to ensure the linear
#' predictor \eqn{\eta} remains strictly positive (or strictly negative) during optimization to avoid division by zero
#' or mapping to invalid negative parameter values.
#'
#' @return An object of class \code{"link"} (see \code{\link{link-class}}) containing
#' the transformation functions and their exact analytical first and second derivatives.
#'
#' @seealso \code{\link{link-class}}, \code{\link{log_link}}, \code{\link{identity_link}}
#'
#' @export
inverse_link <- function() {
  o <- list()
  class(o) <- "link"
  o$link_name <- "inverse"

  o$link_bounds <- c(0, Inf)

  o$linkfun <- function(theta) {
    1 / theta
  }
  o$linkinv <- function(eta) {
    1 / eta
  }

  o$theta_eta <- function(eta) {
    -1 / (eta^2)
  }
  o$theta2_eta2 <- function(eta) {
    2 / (eta^3)
  }

  o$eta_theta <- function(theta) {
    -1 / (theta^2)
  }
  o$eta2_theta2 <- function(theta) {
    2 / (theta^3)
  }

  o$link_params <- NULL

  o
}
