test_that("margot_simulate accepts and applies shadows", {
  # create a simple shadow
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "t1_l",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  # generate data with shadow
  dat <- margot_simulate(
    n = 100,
    waves = 2,
    shadows = shadow,
    seed = 123
  )
  
  # check that shadow was applied
  expect_true("t1_l_true" %in% names(dat))
  expect_true(attr(dat, "margot_meta")$shadows_applied)
  
  # check that variance increased due to measurement error
  expect_gt(var(dat$t1_l, na.rm = TRUE), var(dat$t1_l_true, na.rm = TRUE))
})

test_that("margot_simulate handles multiple shadows", {
  # create multiple shadows
  shadows <- list(
    create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t1_l",
        error_type = "classical",
        sigma = 0.3
      )
    ),
    create_item_missingness_shadow(
      variables = "t2_l",
      missing_rate = 0.2,
      missing_mechanism = "MCAR"
    )
  )
  
  # generate data
  dat <- margot_simulate(
    n = 200,
    waves = 2,
    shadows = shadows,
    seed = 456
  )
  
  # check both shadows were applied
  expect_true("t1_l_true" %in% names(dat))
  expect_true(any(is.na(dat$t2_l)))
  
  # check metadata
  expect_true(attr(dat, "margot_meta")$shadows_applied)
  expect_equal(attr(dat, "applied_shadows"), 
               list("measurement_error_shadow", "item_missingness"))
})

test_that("margot_simulate works with shadows in long format", {
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = c("t1_l", "t2_l"),
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  # generate long format data with shadow
  dat_long <- margot_simulate(
    n = 50,
    waves = 2,
    shadows = shadow,
    wide = FALSE,
    seed = 789
  )
  
  # check that shadow was applied (true values would be preserved in wide before conversion)
  # In long format, we should have l column with measurement error applied
  expect_true("l" %in% names(dat_long))
  expect_equal(attr(dat_long, "margot_meta")$format, "long")
  expect_true(attr(dat_long, "margot_meta")$shadows_applied)
})

test_that("margot_simulate with shadows and interventions", {
  # create misclassification shadow on binary treatment variable
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "t1_a",
      error_type = "misclassification",
      sensitivity = 0.95,  # 95% of true 1s correctly classified
      specificity = 0.90   # 90% of true 0s correctly classified
    )
  )
  
  # intervention function
  always_treat <- function(data, time, trt) {
    rep(1, nrow(data))
  }
  
  # generate data with both intervention and shadow
  dat <- margot_simulate(
    n = 100,
    waves = 2,
    intervention = always_treat,
    shadows = shadow,
    seed = 321
  )
  
  # check that intervention was applied (all should be treated in truth)
  # Note: shadow only applied to t1_a, not t0_a
  expect_true(all(dat$t0_a == 1 | is.na(dat$t0_a)))
  expect_true(all(dat$t1_a_true == 1 | is.na(dat$t1_a_true)))
  
  # but observed values should have some misclassification
  # with 95% sensitivity, about 5% of true 1s will be observed as 0
  expect_true(mean(dat$t1_a[!is.na(dat$t1_a)]) < 1.0)
  expect_true(mean(dat$t1_a[!is.na(dat$t1_a)]) > 0.9)
  
  # check that shadow was applied
  expect_true("t1_a_true" %in% names(dat))
  
  # check metadata
  expect_true(attr(dat, "margot_meta")$intervention_applied)
  expect_true(attr(dat, "margot_meta")$shadows_applied)
})

test_that("margot_simulate handles positivity shadow filtering", {
  # create positivity shadow that filters based on baseline
  pos_shadow <- create_positivity_shadow(
    exposure_var = "t1_a",
    filter_fn = function(data) {
      # only allow treatment if b1 > 0
      data$b1 > 0
    }
  )
  
  dat <- margot_simulate(
    n = 200,
    waves = 2,
    shadows = pos_shadow,
    seed = 654
  )
  
  # should have fewer rows due to filtering
  expect_lt(nrow(dat), 200)
  
  # all remaining should have b1 > 0
  expect_true(all(dat$b1 > 0))
  
  # check positivity filter attribute
  filter_info <- attr(dat, "positivity_filter")
  expect_equal(filter_info$n_original, 200)
})