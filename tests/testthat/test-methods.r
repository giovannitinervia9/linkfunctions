test_that("print.link displays correct information", {
  # Case 1: Finite bounds (Logit: 0 to 1)
  # We verify that the output string matches the expected format exactly.
  # Note: Parentheses ( ) must be escaped with \\ in regex.
  l_logit <- logit_link()
  expect_output(print(l_logit), "logit link for parameters in \\(0, 1\\)")

  # Case 2: Semi-infinite bounds (Log: 0 to Inf)
  l_log <- log_link()
  expect_output(print(l_log), "log link for parameters in \\(0, Inf\\)")

  # Case 3: Custom bounds (Bounded: 10 to 20)
  l_b <- bounded_link(10, 20)
  expect_output(print(l_b), "bounded\\(lwr=10, upr=20\\) link for parameters in \\(10, 20\\)")
})

test_that("plot.link runs without errors", {
  # Set up a temporary graphics device to prevent plot windows from popping up during tests.
  # This ensures the test suite runs silently in the background (CI/CD friendly).
  tmp_file <- tempfile(fileext = ".pdf")
  pdf(file = tmp_file)

  # Ensure the device is closed even if the test fails
  on.exit({
    dev.off()
    unlink(tmp_file)
  })

  # Case 1: Plotting a standard probability link (Finite bounds)
  l_logit <- logit_link()
  expect_error(plot(l_logit), NA) # NA expects NO error

  # Case 2: Plotting a positive link (Semi-infinite bounds)
  l_log <- log_link()
  expect_error(plot(l_log), NA)

  # Case 3: Plotting a bounded link (Custom bounds)
  l_bound <- bounded_link(-5, 5)
  expect_error(plot(l_bound), NA)

  # Case 4: Plotting Rhobit link
  l_rho <- rhobit_link()
  expect_error(plot(l_rho), NA)
})
