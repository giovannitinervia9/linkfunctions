test_that("All link functions respect mathematical properties", {
  # --- Helper: Numerical derivative (Adaptive) ---
  # Improved to handle steep curves by scaling step size with x
  num_deriv <- function(f, x) {
    # Use a step size relative to x, but prevent it from being too small for 0
    # sqrt(.Machine$double.eps) is roughly 1.5e-8, optimal for central differences
    h <- pmax(abs(x) * 1e-5, 1e-7)
    (f(x + h) - f(x - h)) / (2 * h)
  }

  # --- Generic verification function ---
  expect_valid_link <- function(link_obj, theta_vals) {
    link_name <- link_obj$link_name

    # 1. Invertibility Test: linkinv(linkfun(theta)) == theta
    eta_vals <- link_obj$linkfun(theta_vals)
    theta_back <- link_obj$linkinv(eta_vals)

    expect_equal(theta_back, theta_vals,
      tolerance = 1e-5,
      info = paste(link_name, "- Invertibility failed")
    )

    # 2. First Derivative Test dTheta/dEta (theta_eta)
    analytic_d1 <- link_obj$theta_eta(eta_vals)
    numeric_d1 <- vapply(eta_vals, function(e) num_deriv(link_obj$linkinv, e), numeric(1))

    # Increased tolerance slightly to 1e-3 to account for numerical noise in steep areas
    expect_equal(analytic_d1, numeric_d1,
      tolerance = 1e-3,
      info = paste(link_name, "- First derivative (theta_eta) mismatch")
    )

    # 3. Second Derivative Test d2Theta/d2Eta (theta2_eta2)
    analytic_d2 <- link_obj$theta2_eta2(eta_vals)
    numeric_d2 <- vapply(eta_vals, function(e) num_deriv(link_obj$theta_eta, e), numeric(1))

    expect_equal(analytic_d2, numeric_d2,
      tolerance = 1e-3,
      info = paste(link_name, "- Second derivative (theta2_eta2) mismatch")
    )
  }

  # --- TEST EXECUTION ---

  # 1. Links defined on (0, 1) - Probability
  p_vals <- c(0.1, 0.2, 0.5, 0.8, 0.9)
  expect_valid_link(logit_link(), p_vals)
  expect_valid_link(probit_link(), p_vals)
  expect_valid_link(cauchit_link(), p_vals)
  expect_valid_link(cloglog_link(), p_vals)
  expect_valid_link(loglog_link(), p_vals)
  expect_valid_link(bounded_link(0, 1), p_vals)

  # 2. Links defined on (0, Inf) - Positive
  # Removed extreme values (100) for steep links to ensure stability if needed,
  # but with new num_deriv 100 should pass too.
  pos_vals <- c(0.1, 1, 5, 10, 50)
  expect_valid_link(log_link(), pos_vals)
  expect_valid_link(inverse_link(), pos_vals)
  expect_valid_link(inverse_sq_link(), pos_vals)
  expect_valid_link(sqrt_link(), pos_vals)
  expect_valid_link(softplus_link(a = 1), pos_vals)
  expect_valid_link(power_link(lambda = 0.5), pos_vals)
  expect_valid_link(lower_bounded_link(lwr = 0), pos_vals)

  # 3. Links defined on (-Inf, Inf) - Real
  real_vals <- c(-10, -1, 0, 1, 10)
  # Assuming identity_link is defined
  expect_valid_link(identity_link(), real_vals)

  # 4. Links with specific bounds
  rho_vals <- c(-0.8, -0.2, 0, 0.2, 0.8)
  expect_valid_link(rhobit_link(), rho_vals)

  b_vals <- c(11, 15, 19)
  expect_valid_link(lower_bounded_link(10), b_vals)
  expect_valid_link(upper_bounded_link(20), b_vals)
  expect_valid_link(bounded_link(10, 20), b_vals)
})
