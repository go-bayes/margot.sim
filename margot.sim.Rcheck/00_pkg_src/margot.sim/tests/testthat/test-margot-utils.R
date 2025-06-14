test_that("margot_report_sim works correctly", {
  # generate test data
  dat <- margot_simulate(n = 100, waves = 2, seed = 123, verbose = FALSE)
  
  # capture output - cli outputs to message stream
  expect_message(margot_report_sim(dat), "margot simulation summary")
  
  # check error for non-margot data
  bad_data <- data.frame(x = 1:10)
  expect_error(margot_report_sim(bad_data), "does not appear to be from margot_simulate")
  
  # check return value
  suppressMessages({
    meta <- margot_report_sim(dat)
  })
  expect_type(meta, "list")
  expect_equal(meta$n, 100)
  expect_equal(meta$waves, 2)
})

test_that("margot_convert_format works between wide and long", {
  # generate test data in wide format
  dat_wide <- margot_simulate(n = 50, waves = 2, seed = 123, wide = TRUE)
  
  # convert to long
  dat_long <- margot_convert_format(dat_wide, "long")
  expect_s3_class(dat_long, "data.frame")
  expect_true("time" %in% names(dat_long))
  # With waves=2, we have t0, t1, t2, t3 (includes final outcome), so 4 time points
  expect_equal(nrow(dat_long), nrow(dat_wide) * 4)  # 4 time points (0, 1, 2, 3)
  expect_equal(attr(dat_long, "margot_meta")$format, "long")
  
  # convert back to wide
  dat_wide2 <- margot_convert_format(dat_long, "wide")
  expect_equal(nrow(dat_wide2), nrow(dat_wide))
  expect_equal(attr(dat_wide2, "margot_meta")$format, "wide")
  
  # check idempotency
  expect_message(
    dat_wide3 <- margot_convert_format(dat_wide2, "wide"),
    "already in wide format"
  )
  expect_identical(dat_wide2, dat_wide3)
  
  # check metadata preservation
  meta_orig <- attr(dat_wide, "margot_meta")
  meta_converted <- attr(dat_long, "margot_meta")
  expect_equal(meta_orig$n, meta_converted$n)
  expect_equal(meta_orig$waves, meta_converted$waves)
  
  # test with sampling weights
  dat_weighted <- margot_simulate(
    n = 50, 
    waves = 2, 
    seed = 123,
    sampling_weights = runif(50, 0.5, 2)
  )
  dat_weighted_long <- margot_convert_format(dat_weighted, "long")
  expect_true("sampling_weight" %in% names(dat_weighted_long))
})

test_that("margot_convert_format handles errors properly", {
  # data without metadata
  bad_data <- data.frame(id = 1:10, t0_a = rbinom(10, 1, 0.5))
  expect_error(margot_convert_format(bad_data, "long"), "must have margot_meta attribute")
})

test_that("margot_extract_var extracts correct values", {
  dat <- margot_simulate(n = 100, waves = 2, seed = 123)
  
  # extract existing variable
  a1 <- margot_extract_var(dat, "a", 1)
  expect_length(a1, 100)
  expect_equal(a1, dat$t1_a)
  
  # extract baseline covariate (should fail)
  expect_error(margot_extract_var(dat, "b1", 0), "not found")
  
  # extract non-existent variable
  expect_error(margot_extract_var(dat, "z", 1), "not found")
  
  # extract outcome
  y2 <- margot_extract_var(dat, "y", 2)
  expect_equal(y2, dat$t2_y)
})

test_that("margot_missingness_summary provides correct summary", {
  # generate data with some missingness
  dat <- margot_simulate(
    n = 200, 
    waves = 3,
    censoring = list(rate = 0.2),
    seed = 123
  )
  
  # get summary
  miss_summ <- margot_missingness_summary(dat)
  
  # check structure
  expect_s3_class(miss_summ, "data.frame")
  expect_named(miss_summ, c("variable", "n_missing", "prop_missing", "time", "var_name"))
  
  # check ordering
  expect_true(all(diff(miss_summ$time) >= 0))
  
  # check values make sense
  expect_true(all(miss_summ$n_missing >= 0))
  expect_true(all(miss_summ$n_missing <= 200))
  expect_true(all(miss_summ$prop_missing >= 0))
  expect_true(all(miss_summ$prop_missing <= 1))
  
  # check specific variables
  miss_vars <- c("t1_a", "t2_a", "t1_y", "t2_y")
  miss_subset <- margot_missingness_summary(dat, vars = miss_vars)
  expect_equal(nrow(miss_subset), 4)
  # The function sorts by time and var_name, so order may differ
  expect_setequal(miss_subset$variable, miss_vars)
  
  # check with no missingness
  dat_complete <- margot_simulate(
    n = 50,
    waves = 2,
    censoring = list(rate = 0),
    seed = 456
  )
  miss_complete <- margot_missingness_summary(dat_complete)
  expect_true(all(miss_complete$n_missing == 0))
  expect_true(all(miss_complete$prop_missing == 0))
})

test_that("margot utilities handle edge cases", {
  # single wave data
  dat_single <- margot_simulate(n = 30, waves = 1, seed = 789)
  
  # report should work
  expect_message(margot_report_sim(dat_single), "waves = 1")
  
  # convert format should work
  dat_single_long <- margot_convert_format(dat_single, "long")
  # With waves=1, we have t0, t1, t2 (includes final outcome), so 3 time points
  expect_equal(nrow(dat_single_long), 30 * 3)  # t0, t1, and t2
  
  # missingness summary should work
  miss_single <- margot_missingness_summary(dat_single)
  expect_true(nrow(miss_single) > 0)
  
  # extract var should work
  a0_vals <- margot_extract_var(dat_single, "a", 0)
  expect_length(a0_vals, 30)
})

test_that("utilities preserve margot metadata", {
  # create data
  dat <- margot_simulate(n = 50, waves = 2, seed = 111)
  
  # get original metadata
  meta_orig <- attr(dat, "margot_meta")
  
  # convert format should preserve margot_meta
  dat_long <- margot_convert_format(dat, "long")
  meta_long <- attr(dat_long, "margot_meta")
  expect_equal(meta_long$n, meta_orig$n)
  expect_equal(meta_long$waves, meta_orig$waves)
  expect_equal(meta_long$format, "long")
  
  dat_wide_again <- margot_convert_format(dat_long, "wide")
  meta_wide <- attr(dat_wide_again, "margot_meta")
  expect_equal(meta_wide$n, meta_orig$n)
  expect_equal(meta_wide$waves, meta_orig$waves)
  expect_equal(meta_wide$format, "wide")
})