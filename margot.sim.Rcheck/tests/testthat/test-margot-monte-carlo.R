test_that("margot_monte_carlo runs basic simulation", {
  # simple OLS estimator
  ols_estimator <- function(data) {
    # check if we have the expected columns
    if (!all(c("t2_y", "t1_a", "b1") %in% names(data))) {
      return(list(estimate = NA, se = NA, converged = FALSE))
    }
    
    fit <- lm(t2_y ~ t1_a + b1, data = data)
    
    list(
      estimate = coef(fit)["t1_a"],
      se = sqrt(diag(vcov(fit)))["t1_a"],
      converged = TRUE
    )
  }
  
  # run small monte carlo
  mc_results <- margot_monte_carlo(
    n_reps = 10,
    n_per_rep = 100,
    dgp_params = list(
      waves = 1,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]]
      ),
      params = list(a_lag_y_coef = 0.3)
    ),
    estimator_fn = ols_estimator,
    truth_fn = function(data) 0.3,
    seed = 123,
    verbose = FALSE
  )
  
  expect_s3_class(mc_results, "margot_mc_results")
  expect_equal(nrow(mc_results$results), 10)
  expect_true(all(mc_results$results$converged))
  
  # check performance metrics
  expect_true(abs(mc_results$performance$bias) < 0.5)  # reasonable bias
  expect_equal(mc_results$performance$n_valid, 10)
})

test_that("margot_monte_carlo handles estimator failures", {
  # estimator that fails sometimes
  failing_estimator <- function(data) {
    if (runif(1) > 0.7) {
      # Return consistent structure even on failure
      return(list(
        estimate = NA,
        se = NA,
        converged = FALSE
      ))
    }
    
    list(
      estimate = rnorm(1),
      se = 0.1,
      converged = TRUE
    )
  }
  
  mc_results <- margot_monte_carlo(
    n_reps = 20,
    n_per_rep = 50,
    dgp_params = list(
      waves = 1,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]]
      )
    ),
    estimator_fn = failing_estimator,
    seed = 123,
    verbose = FALSE
  )
  
  # some should have failed
  expect_lt(mc_results$performance$convergence_rate, 1)
  expect_true(any(!mc_results$results$converged))
})

test_that("margot_monte_carlo applies shadows correctly", {
  simple_estimator <- function(data) {
    # estimate mean of t1_l
    est <- mean(data$t1_l, na.rm = TRUE)
    list(
      estimate = est,
      se = sd(data$t1_l, na.rm = TRUE) / sqrt(sum(!is.na(data$t1_l))),
      converged = TRUE
    )
  }
  
  # create measurement error shadow
  shadow <- create_shadow(
    "measurement_error",
    params = list(
      variables = "t1_l",
      error_type = "classical",
      sigma = 1
    )
  )
  
  # run with shadow
  mc_with_shadow <- margot_monte_carlo(
    n_reps = 50,
    n_per_rep = 100,
    dgp_params = list(
      waves = 1,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]]
      )
    ),
    shadows = list(shadow),
    estimator_fn = simple_estimator,
    seed = 123,
    verbose = FALSE
  )
  
  # run without shadow
  mc_no_shadow <- margot_monte_carlo(
    n_reps = 50,
    n_per_rep = 100,
    dgp_params = list(
      waves = 1,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]]
      )
    ),
    shadows = list(),
    estimator_fn = simple_estimator,
    seed = 456,
    verbose = FALSE
  )
  
  # compare the variance of estimates across replications
  var_with_shadow <- var(mc_with_shadow$results$estimate, na.rm = TRUE)
  var_no_shadow <- var(mc_no_shadow$results$estimate, na.rm = TRUE)
  
  # variance should be higher with measurement error (more noise)
  expect_gt(var_with_shadow, var_no_shadow)
})

test_that("compare_mc_results works", {
  simple_est <- function(data) {
    list(estimate = rnorm(1, 0.3, 0.1), se = 0.1, converged = TRUE)
  }
  
  # create two MC results
  mc1 <- margot_monte_carlo(
    n_reps = 20,
    n_per_rep = 100,
    dgp_params = list(
      waves = 1,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]]
      )
    ),
    estimator_fn = simple_est,
    truth_fn = function(data) 0.3,
    seed = 123,
    verbose = FALSE
  )
  
  mc2 <- margot_monte_carlo(
    n_reps = 20,
    n_per_rep = 100,
    dgp_params = list(
      waves = 1,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]]
      )
    ),
    estimator_fn = simple_est,
    truth_fn = function(data) 0.3,
    seed = 456,
    verbose = FALSE
  )
  
  # compare
  comparison <- compare_mc_results(mc1, mc2, names = c("Method A", "Method B"))
  
  expect_s3_class(comparison, "mc_comparison")
  expect_equal(nrow(comparison), 2)
  expect_equal(comparison$method, c("Method A", "Method B"))
})