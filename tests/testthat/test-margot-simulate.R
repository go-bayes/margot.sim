test_that("margot_simulate creates data with correct dimensions", {
  # basic simulation
  dat <- margot_simulate(n = 100, waves = 3, seed = 123)
  
  expect_equal(nrow(dat), 100)
  expect_true("id" %in% names(dat))
  expect_true("b1" %in% names(dat))
  expect_true("t0_a" %in% names(dat))
  expect_true("t3_a" %in% names(dat))
  expect_true("t4_y" %in% names(dat))
})

test_that("margot_simulate respects outcome and baseline settings", {
  # multiple outcomes
  dat <- margot_simulate(n = 50, waves = 2, n_outcomes = 2, seed = 123)
  expect_true("t3_y1" %in% names(dat))
  expect_true("t3_y2" %in% names(dat))
  
  # custom baselines
  dat <- margot_simulate(n = 50, waves = 2, n_baselines = 3, seed = 123)
  expect_true(all(c("b1", "b2", "b3") %in% names(dat)))
  expect_false("b4" %in% names(dat))
})

test_that("margot_simulate handles different exposure types", {
  # binary exposure
  dat_bin <- margot_simulate(n = 100, waves = 2, exposure_type = "binary", seed = 123)
  expect_true(all(dat_bin$t1_a %in% c(0, 1, NA)))
  
  # continuous exposure
  dat_cont <- margot_simulate(n = 100, waves = 2, exposure_type = "continuous", seed = 123)
  expect_true(is.numeric(dat_cont$t1_a))
  expect_false(all(dat_cont$t1_a %in% c(0, 1, NA)))
})

test_that("margot_simulate applies interventions correctly", {
  # always treat intervention
  always_treat <- function(data, time, trt) {
    rep(1, nrow(data))
  }
  
  dat <- margot_simulate(
    n = 100, 
    waves = 2, 
    intervention = always_treat,
    seed = 123
  )
  
  # check that all exposures are 1 (except possibly NA due to censoring)
  expect_true(all(dat$t0_a == 1 | is.na(dat$t0_a)))
  expect_true(all(dat$t1_a == 1 | is.na(dat$t1_a)))
})

test_that("margot_simulate preserves metadata", {
  dat <- margot_simulate(n = 50, waves = 2, seed = 123)
  meta <- attr(dat, "margot_meta")
  
  expect_equal(meta$n, 50)
  expect_equal(meta$waves, 2)
  expect_equal(meta$structural_model, "semi-markovian")
  expect_false(meta$intervention_applied)
})

test_that("margot_simulate handles long format", {
  dat_long <- margot_simulate(n = 50, waves = 2, wide = FALSE, seed = 123)
  
  expect_true("time" %in% names(dat_long))
  # with 2 waves, we have t0, t1, t2, t3 (waves + 2 time points)
  expect_equal(nrow(dat_long), 50 * 4) # t0 through t3
  expect_equal(attr(dat_long, "margot_meta")$format, "long")
})