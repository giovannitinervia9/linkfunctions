#' Print Method for Link Objects
#'
#' @description
#' A standard S3 print method for objects of class \code{"link"}.
#' It displays the name of the link function and the valid domain of the parameter \eqn{\theta}.
#'
#' @param x An object of class \code{"link"}.
#' @param ... Additional arguments passed to methods (currently unused).
#'
#' @return The function returns \code{x} invisibly.
#'
#' @export
#' @method print link
print.link <- function(x, ...) {
  cat(x$link_name,
    " link for parameters in (",
    x$link_bounds[1],
    ", ",
    x$link_bounds[2],
    ")\n",
    sep = ""
  )
}




#' Visualize Link Functions
#'
#' @description
#' A standard S3 plot method for objects of class \code{"link"}.
#' It generates a panel with two plots:
#' \enumerate{
#'   \item The link function \eqn{\eta = g(\theta)} over its valid domain.
#'   \item The inverse link function \eqn{\theta = g^{-1}(\eta)} over a standard range of linear predictors.
#' }
#'
#' @param x An object of class \code{"link"}.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}} (currently unused).
#'
#' @details
#' The function automatically handles the x-axis limits based on whether the link bounds are finite or infinite.
#' It temporarily modifies the graphical parameters (\code{par}) to create a side-by-side layout and restores
#' the original settings upon exit.
#'
#' @importFrom graphics par plot grid abline mtext
#'
#' @return No return value, called for side effects (plotting).
#'
#' @export
#' @method plot link
plot.link <- function(x, ...) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(
    mfrow = c(1, 2),
    mar = c(5, 5, 3.5, 2) + 0.1,
    oma = c(0, 0, 2, 0)
  )

  lb <- x$link_bounds[1]
  ub <- x$link_bounds[2]
  eps <- 1e-02

  if (is.finite(lb)) {
    from_theta <- lb + eps
  } else {
    from_theta <- -3
  }

  if (is.finite(ub)) {
    to_theta <- ub - eps
  } else {
    to_theta <- from_theta + 3
  }

  theta <- seq(
    from_theta,
    to_theta,
    l = 1000
  )

  plot(
    theta,
    x$linkfun(theta),
    main = "Link function",
    xlab = expression(theta),
    ylab = expression(eta == g(theta)),
    lwd = 2,
    las = 1,
    type = "l"
  )
  grid()
  abline(0, 1, lty = 2, col = "grey")

  from_eta <- min(x$linkfun(theta))
  to_eta <- max(x$linkfun(theta))
  eta <- seq(
    from_eta,
    to_eta,
    l = 1000
  )

  plot(
    eta,
    x$linkinv(eta),
    main = "Inverse link function",
    xlab = expression(eta),
    ylab = expression(theta == g^
      {
        -1
      } * (eta)),
    lwd = 2,
    las = 1,
    type = "l"
  )
  grid()
  abline(0, 1, lty = 2, col = "grey")

  mtext(paste("Link:", x$link_name), side = 3, line = 0, outer = TRUE, cex = 1.2, font = 2)
}
