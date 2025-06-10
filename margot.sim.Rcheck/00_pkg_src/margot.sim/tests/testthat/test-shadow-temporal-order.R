library(margot.sim)

test_that("temporal order is enforced in shadow dependencies", {
  # Should pass - t2 variable depends on t1 variable
  expect_silent(
    create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t2_a",
        error_type = "differential",
        differential_var = "t1_l",
        differential_fn = function(x) abs(x) * 0.1
      )
    )
  )
  
  # Should fail - t1 variable cannot depend on t2 variable
  expect_error(
    create_shadow(
      type = "measurement_error", 
      params = list(
        variables = "t1_a",
        error_type = "differential",
        differential_var = "t2_l",
        differential_fn = function(x) abs(x) * 0.1
      )
    ),
    "Temporal order violation"
  )
})

test_that("temporal order is enforced for item missingness", {
  # Should pass - MAR depending on past variables
  expect_silent(
    create_item_missingness_shadow(
      variables = c("t2_y", "t3_y"),
      missing_rate = 0.2,
      missing_mechanism = "MAR",
      dependent_vars = c("t1_a", "b1")
    )
  )
  
  # Should fail - MAR depending on future variables
  expect_error(
    create_item_missingness_shadow(
      variables = "t1_y",
      missing_rate = 0.2,
      missing_mechanism = "MAR",
      dependent_vars = c("t2_a", "t3_l")
    ),
    "Temporal order violation"
  )
  
  # MCAR should not require temporal validation
  expect_silent(
    create_item_missingness_shadow(
      variables = "t1_y",
      missing_rate = 0.2,
      missing_mechanism = "MCAR"
    )
  )
})

test_that("get_time_index extracts time correctly", {
  expect_equal(margot.sim:::get_time_index("t0_a"), 0)
  expect_equal(margot.sim:::get_time_index("t1_exposure"), 1)
  expect_equal(margot.sim:::get_time_index("t10_y"), 10)
  expect_true(is.na(margot.sim:::get_time_index("b1")))
  expect_true(is.na(margot.sim:::get_time_index("baseline_var")))
})

test_that("validate_temporal_order works correctly", {
  # Valid: same time dependencies
  expect_silent(
    margot.sim:::validate_temporal_order(
      target_vars = c("t2_y"),
      dependency_vars = c("t2_a", "t1_l", "b1")
    )
  )
  
  # Valid: past dependencies
  expect_silent(
    margot.sim:::validate_temporal_order(
      target_vars = c("t3_y"),
      dependency_vars = c("t1_a", "t2_l")
    )
  )
  
  # Invalid: future dependencies
  expect_error(
    margot.sim:::validate_temporal_order(
      target_vars = c("t1_y"),
      dependency_vars = c("t2_a", "t3_l")
    ),
    "Temporal order violation"
  )
  
  # Baseline variables can depend on other baseline variables
  expect_silent(
    margot.sim:::validate_temporal_order(
      target_vars = c("b1", "b2"),
      dependency_vars = c("b3", "b4")
    )
  )
})

test_that("shadows respect causal temporal order in practice", {
  # Generate test data
  set.seed(123)
  data <- margot_simulate(
    n = 100,
    waves = 3,
    apply_process_function = FALSE
  )
  
  # Create shadow with proper temporal dependencies
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = c("t2_a", "t3_a"),
      error_type = "differential",
      differential_var = "t1_l",  # Past variable affects future measurement
      differential_fn = function(x) pmax(0.1, abs(x) * 0.2)
    )
  )
  
  # Apply shadow
  shadowed_data <- apply_shadow(data, shadow)
  
  # Check that original values are preserved
  expect_true("t2_a_true" %in% names(shadowed_data))
  expect_true("t3_a_true" %in% names(shadowed_data))
  
  # Check that error depends on past state
  error_t2 <- shadowed_data$t2_a - shadowed_data$t2_a_true
  error_t3 <- shadowed_data$t3_a - shadowed_data$t3_a_true
  
  # Variance should be related to t1_l values
  high_t1_l <- abs(data$t1_l) > median(abs(data$t1_l))
  expect_true(var(error_t2[high_t1_l]) > var(error_t2[!high_t1_l]))
})