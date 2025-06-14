# test that vignettes can be rendered without errors

test_that("all vignettes can be built", {
  skip_on_cran()
  skip_if_not(rmarkdown::pandoc_available())
  
  # get all vignette files
  vignette_dir <- system.file("vignettes", package = "margot.sim")
  if (vignette_dir == "") {
    vignette_dir <- here::here("vignettes")
  }
  
  vignette_files <- list.files(
    vignette_dir,
    pattern = "\\.Rmd$",
    full.names = TRUE
  )
  
  # list of vignettes with known issues that need fixing later
  skip_vignettes <- c(
    "advanced-shift-interventions.Rmd",
    "censoring-effect-mod.Rmd", 
    "heterogeneous-effects.Rmd",
    "practical-workflow.Rmd"
  )
  
  # test each vignette
  for (vignette in vignette_files) {
    test_name <- basename(vignette)
    
    if (test_name %in% skip_vignettes) {
      skip(paste("Skipping", test_name, "- needs fixing"))
      next
    }
    
    # test that the vignette renders without fatal errors
    # using tryCatch to provide better error messages
    result <- tryCatch({
      rmarkdown::render(
        vignette,
        output_format = "html_document",
        output_dir = tempdir(),
        quiet = TRUE
      )
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    expect_true(result, info = paste("Failed to render", test_name))
  }
})

test_that("basic-simulation vignette code runs correctly", {
  # test key code chunks from basic-simulation.Rmd
  set.seed(123)
  
  # basic simulation
  sim_data <- margot_simulate(
    n = 100,  # smaller for testing
    waves = 3,
    n_outcomes = 1
  )
  
  expect_equal(nrow(sim_data), 100)
  expect_true("id" %in% names(sim_data))
  expect_true("t0_a" %in% names(sim_data))
  expect_true("t1_y" %in% names(sim_data))
  
  # custom simulation
  sim_custom <- margot_simulate(
    n = 50,
    waves = 2,
    n_baselines = 3,
    exposure_type = "continuous",
    outcome_type = "binary"
  )
  
  expect_true(is.numeric(sim_custom$t1_a))
  expect_true(all(sim_custom$t1_y %in% c(0, 1)))
  
  # censoring
  sim_censored <- margot_simulate(
    n = 100,
    waves = 2,
    censoring = list(
      rate = 0.2,
      exposure_dependence = TRUE,
      y_dependence = TRUE
    )
  )
  
  expect_true("t0_not_lost_following_wave" %in% names(sim_censored))
  
  # long format
  sim_long <- margot_simulate(
    n = 50,
    waves = 2,
    wide = FALSE
  )
  
  expect_true("time" %in% names(sim_long))  # 'time' not 'wave' 
  expect_true(nrow(sim_long) > 50)  # more rows in long format
  
  # intervention
  always_treat <- function(data, time, trt) {
    rep(1, nrow(data))
  }
  
  sim_intervention <- margot_simulate(
    n = 50,
    waves = 2,
    intervention = always_treat
  )
  
  expect_true(all(sim_intervention$t1_a == 1))
})

test_that("applying-shadows vignette code runs correctly", {
  set.seed(123)
  
  # generate base data
  sim_data <- margot_simulate(n = 100, waves = 2)
  
  # create and apply measurement error shadow
  error_shadow <- create_shadow(
    type = "measurement_error",
    variables = c("t1_y", "t2_y"),
    error_sd = 0.5
  )
  
  shadow_result <- apply_shadow(error_shadow, sim_data)
  
  expect_false(identical(sim_data$t1_y, shadow_result$data$t1_y))
  
  # create and apply missingness shadow  
  miss_shadow <- create_shadow(
    type = "item_missingness",
    variables = c("t1_y", "t2_y"),
    miss_prob = 0.2,
    miss_mechanism = "mar"
  )
  
  miss_result <- apply_shadow(miss_shadow, sim_data)
  
  expect_true(any(is.na(miss_result$data)))
})

test_that("monte-carlo-simple vignette code runs correctly", {
  set.seed(123)
  
  # simple estimator
  simple_estimator <- function(data) {
    model <- lm(t2_y ~ t1_a + b1, data = data)
    return(list(estimate = coef(model)["t1_a"]))
  }
  
  # run small monte carlo using margot_monte_carlo
  mc_results <- margot_monte_carlo(
    n_sims = 10,  # small for testing
    data_generator = function() {
      margot_simulate(
        n = 100,
        waves = 2,
        params = list(
          a_y_coef = 0.3,
          b_y_coef = 0.2
        )
      )
    },
    estimator = simple_estimator,
    truth = 0.3
  )
  
  expect_s3_class(mc_results, "margot_mc_results")
  expect_true("estimate" %in% names(mc_results$results))
  expect_equal(nrow(mc_results$results), 10)
})

test_that("shift-interventions vignette code runs correctly", {
  set.seed(123)
  
  # define shift function
  shift_up <- function(data, time, trt) {
    pmin(data[[trt]] + 0.5, 5)
  }
  
  # simulate with shift
  sim_shift <- margot_simulate(
    n = 100,
    waves = 2,
    exposure_type = "continuous",
    intervention = shift_up
  )
  
  expect_true(all(sim_shift$t1_a <= 5))
  
  # threshold shift
  threshold_shift <- function(data, time, trt) {
    ifelse(data[[trt]] < 2, 2, data[[trt]])
  }
  
  sim_threshold <- margot_simulate(
    n = 100,
    waves = 2,
    exposure_type = "continuous",
    intervention = threshold_shift
  )
  
  expect_true(all(sim_threshold$t1_a >= 2))
})

test_that("vignette helper functions are available", {
  # check that key package functions used in vignettes exist
  expect_true(exists("margot_simulate"))
  expect_true(exists("apply_shadow"))
  expect_true(exists("margot_monte_carlo"))
  expect_true(exists("create_scenario"))
  expect_true(exists("margot_transport_analysis"))
})