# Test shift intervention functions

test_that("create_shift_intervention creates valid shift functions", {
  # create a bounded shift
  shift_fn <- create_shift_intervention(
    shift_amount = 2,
    min_value = 0,
    max_value = 10,
    start_wave = 1
  )
  
  # test data
  data <- data.frame(
    id = 1:5,
    treatment = c(-1, 3, 5, 9, 12)
  )
  
  # before start wave - should return original
  result <- shift_fn(data, time = 0, trt = "treatment")
  expect_equal(result, data$treatment)
  
  # at/after start wave - should shift with bounds
  result <- shift_fn(data, time = 1, trt = "treatment")
  expected <- c(0, 5, 7, 10, 10)  # shifted and bounded
  expect_equal(result, expected)
})

test_that("create_shift_intervention handles edge cases", {
  # no bounds
  shift_fn <- create_shift_intervention(shift_amount = -1)
  
  data <- data.frame(x = c(0, 5, 10))
  result <- shift_fn(data, time = 1, trt = "x")
  expect_equal(result, c(-1, 4, 9))
  
  # extreme bounds
  shift_fn <- create_shift_intervention(
    shift_amount = 100,
    min_value = 0,
    max_value = 1
  )
  
  result <- shift_fn(data, time = 1, trt = "x")
  expect_equal(result, c(1, 1, 1))  # all capped at max
})

test_that("create_threshold_shift works for both directions", {
  # shift up
  shift_up <- create_threshold_shift(
    threshold = 5,
    shift_to = 5,
    direction = "up"
  )
  
  data <- data.frame(x = c(2, 4, 6, 8))
  
  # before start wave
  result <- shift_up(data, time = 0, trt = "x")
  expect_equal(result, data$x)
  
  # after start wave - values below 5 shifted to 5
  result <- shift_up(data, time = 1, trt = "x")
  expect_equal(result, c(5, 5, 6, 8))
  
  # shift down
  shift_down <- create_threshold_shift(
    threshold = 5,
    shift_to = 5,
    direction = "down"
  )
  
  result <- shift_down(data, time = 1, trt = "x")
  expect_equal(result, c(2, 4, 5, 5))
})

test_that("create_wave_specific_shift applies correct shifts", {
  # define wave-specific shifts
  wave_shifts <- list(
    "0" = function(x) x,           # no change
    "1" = function(x) x + 1,       # add 1
    "2" = function(x) x * 2,       # double
    "3" = function(x) pmax(x, 5)   # floor at 5
  )
  
  shift_fn <- create_wave_specific_shift(wave_shifts)
  
  data <- data.frame(treatment = c(1, 2, 3, 4))
  
  # test each wave
  expect_equal(shift_fn(data, 0, "treatment"), c(1, 2, 3, 4))     # no change
  expect_equal(shift_fn(data, 1, "treatment"), c(2, 3, 4, 5))     # +1
  expect_equal(shift_fn(data, 2, "treatment"), c(2, 4, 6, 8))     # *2
  expect_equal(shift_fn(data, 3, "treatment"), c(5, 5, 5, 5))     # max with 5
  
  # undefined wave - should return original
  expect_equal(shift_fn(data, 99, "treatment"), c(1, 2, 3, 4))
})

test_that("create_lmtp_style_shifts creates valid shift pair", {
  shifts <- create_lmtp_style_shifts(
    min_score = 1,
    max_score = 7,
    baseline_wave = 0,
    shift_wave = 1
  )
  
  expect_type(shifts, "list")
  expect_true("shift_up" %in% names(shifts))
  expect_true("shift_down" %in% names(shifts))
  expect_true(is.function(shifts$shift_up))
  expect_true(is.function(shifts$shift_down))
  
  # test the functions
  data <- data.frame(score = c(1, 3, 5, 7))
  
  # baseline wave - no change
  expect_equal(shifts$shift_up(data, 0, "score"), data$score)
  expect_equal(shifts$shift_down(data, 0, "score"), data$score)
  
  # shift wave
  expect_equal(shifts$shift_up(data, 1, "score"), c(2, 4, 6, 7))    # bounded at 7
  expect_equal(shifts$shift_down(data, 1, "score"), c(1, 2, 4, 6))  # bounded at 1
  
  # other waves - no change
  expect_equal(shifts$shift_up(data, 2, "score"), data$score)
})

test_that("Shift interventions integrate with margot_simulate", {
  # create intervention
  shift_fn <- create_shift_intervention(
    shift_amount = 1,
    min_value = 0,
    max_value = 10
  )
  
  # use in simulation
  sim <- margot_simulate(
    n = 100,
    waves = 2,
    exposure_type = "continuous",
    intervention = shift_fn,
    params = list(u_shift = 3)  # ensure continuous exposure
  )
  
  # check that intervention was applied
  # natural values should differ from shifted values
  expect_true("cf" %in% names(attributes(sim)))
  expect_false(all(sim$t1_a == attr(sim, "cf")$cf_t1_a))
})

test_that("create_threshold_shift handles missing values", {
  shift_fn <- create_threshold_shift(
    threshold = 5,
    shift_to = 5,
    direction = "up"
  )
  
  data <- data.frame(x = c(2, NA, 6, 8))
  result <- shift_fn(data, 1, "x")
  
  # NA should remain NA
  expect_equal(result, c(5, NA, 6, 8))
})

test_that("create_mtp_intervention works correctly", {
  # test MTP creation
  mtp <- create_mtp_intervention(
    shift_fn = function(a, data) pmin(a + 0.5, 5),
    exposure_vars = c("t0_a", "t1_a", "t2_a")
  )
  
  expect_true(is.function(mtp))
  
  # test with data
  data <- data.frame(
    t0_a = c(1, 2, 3),
    t1_a = c(2, 3, 4),
    t2_a = c(3, 4, 5)
  )
  
  result <- mtp(data, time = 1, trt = "t1_a")
  expect_equal(result, c(2.5, 3.5, 4.5))
  
  # test bounds
  data$t1_a <- c(4.7, 4.8, 4.9)
  result <- mtp(data, time = 1, trt = "t1_a")
  expect_equal(result, c(5, 5, 5))  # capped at 5
})

test_that("create_ips_intervention creates valid weighting function", {
  # create IPS intervention
  ips <- create_ips_intervention(
    policy_prob_fn = function(a, data) {
      # simple policy: prefer higher values
      dnorm(a, mean = 5, sd = 1)
    },
    natural_prob_fn = function(a, data) {
      # natural distribution
      dnorm(a, mean = 3, sd = 2)
    }
  )
  
  expect_true(is.function(ips))
  
  # test weighting
  data <- data.frame(
    id = 1:5,
    treatment = c(2, 3, 4, 5, 6)
  )
  
  weights <- ips(data, time = 1, trt = "treatment")
  expect_equal(length(weights), 5)
  expect_true(all(weights > 0))
  expect_true(all(is.finite(weights)))
})

test_that("Wave-specific shifts handle character wave indices", {
  wave_shifts <- list(
    "baseline" = function(x) x,
    "followup1" = function(x) x + 2,
    "followup2" = function(x) x - 1
  )
  
  shift_fn <- create_wave_specific_shift(wave_shifts)
  
  data <- data.frame(y = c(10, 20, 30))
  
  # test with character time
  expect_equal(shift_fn(data, "baseline", "y"), c(10, 20, 30))
  expect_equal(shift_fn(data, "followup1", "y"), c(12, 22, 32))
  expect_equal(shift_fn(data, "followup2", "y"), c(9, 19, 29))
})

test_that("Shift interventions preserve data types", {
  # integer shift
  shift_int <- create_shift_intervention(shift_amount = 2L)
  data <- data.frame(x = c(1L, 2L, 3L))
  result <- shift_int(data, 1, "x")
  expect_type(result, "integer")
  
  # numeric shift  
  shift_num <- create_shift_intervention(shift_amount = 2.5)
  data <- data.frame(x = c(1, 2, 3))
  result <- shift_num(data, 1, "x")
  expect_type(result, "double")
})

test_that("create_shift_intervention validates inputs", {
  # valid creation
  expect_silent(create_shift_intervention(1))
  
  # bounds validation
  shift_fn <- create_shift_intervention(
    shift_amount = 1,
    min_value = 10,
    max_value = 5  # max < min
  )
  
  data <- data.frame(x = c(7, 8, 9))
  result <- shift_fn(data, 1, "x")
  # should handle gracefully even with invalid bounds
  expect_true(all(is.numeric(result)))
})

test_that("Complex shift patterns work correctly", {
  # create a complex shift that depends on other variables
  complex_shift <- function(data, time, trt) {
    if (time == 0) return(data[[trt]])
    
    # shift based on another covariate
    baseline_cov <- data$baseline_health
    shift_amount <- ifelse(baseline_cov > median(baseline_cov), 2, 1)
    
    data[[trt]] + shift_amount
  }
  
  data <- data.frame(
    treatment = c(1, 2, 3, 4, 5),
    baseline_health = c(10, 20, 30, 40, 50)
  )
  
  result <- complex_shift(data, 1, "treatment")
  expect_equal(result, c(2, 3, 5, 6, 7))  # different shifts based on health
})