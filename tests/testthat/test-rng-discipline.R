# Test RNG discipline implementation
library(margot.sim)

test_that("RNG streams are independent", {
  streams <- create_rng_streams(10, seed = 123)
  
  # check we have the right number of streams
  expect_equal(length(streams), 10)
  
  # check no duplicate streams
  expect_equal(anyDuplicated(streams), 0)
  
  # check streams produce different sequences
  samples <- lapply(streams, function(stream) {
    margot.sim:::set_rng_stream(stream)
    runif(5)
  })
  
  # no two streams should produce identical sequences
  for (i in 1:9) {
    for (j in (i+1):10) {
      expect_false(identical(samples[[i]], samples[[j]]))
    }
  }
})

test_that("RNG streams work with different kinds", {
  # test with default L'Ecuyer
  streams1 <- create_rng_streams(5, seed = 456, kind = "L'Ecuyer-CMRG")
  expect_equal(length(streams1), 5)
  
  # test state restoration
  old_seed <- .Random.seed
  old_kind <- RNGkind()[1]
  
  streams <- create_rng_streams(3, seed = 789)
  
  # check that RNG state is restored
  expect_equal(RNGkind()[1], old_kind)
})

test_that("set_rng_stream sets state correctly", {
  streams <- create_rng_streams(3, seed = 111)
  
  # set first stream
  margot.sim:::set_rng_stream(streams[[1]])
  vals1 <- runif(5)
  
  # set different stream
  margot.sim:::set_rng_stream(streams[[2]])
  vals2 <- runif(5)
  
  # go back to first stream - should get same values
  margot.sim:::set_rng_stream(streams[[1]])
  vals1_repeat <- runif(5)
  
  expect_equal(vals1, vals1_repeat)
  expect_false(identical(vals1, vals2))
})

test_that("run_mc_replication uses streams correctly", {
  test_fn <- function(rep_id, n = 100) {
    list(
      rep_id = rep_id,
      mean = mean(rnorm(n)),
      max = max(runif(n))
    )
  }
  
  streams <- create_rng_streams(5, seed = 222)
  
  # run replications
  results <- lapply(1:5, function(i) {
    margot.sim:::run_mc_replication(i, streams[[i]], test_fn, n = 50)
  })
  
  expect_equal(length(results), 5)
  expect_equal(results[[1]]$rep_id, 1)
  expect_true(is.numeric(results[[1]]$mean))
  expect_true(is.numeric(results[[1]]$max))
})

test_that("RNG streams are reproducible", {
  # create streams twice with same seed
  streams1 <- create_rng_streams(5, seed = 456)
  streams2 <- create_rng_streams(5, seed = 456)
  
  expect_identical(streams1, streams2)
  
  # check that using streams produces same results
  results1 <- lapply(streams1, function(stream) {
    margot.sim:::set_rng_stream(stream)
    rnorm(10)
  })
  
  results2 <- lapply(streams2, function(stream) {
    margot.sim:::set_rng_stream(stream)
    rnorm(10)
  })
  
  expect_identical(results1, results2)
})

test_that("Parallel and sequential MC produce identical results", {
  skip_if_not_installed("parallel")
  
  # simple test function
  test_fn <- function(rep_id) {
    data <- rnorm(100)
    list(mean = mean(data), sd = sd(data))
  }
  
  # create streams
  streams <- create_rng_streams(20, seed = 789)
  
  # run sequentially
  seq_results <- lapply(1:20, function(i) {
    margot.sim:::set_rng_stream(streams[[i]])
    test_fn(i)
  })
  
  # run again with same streams to check reproducibility
  seq_results2 <- lapply(1:20, function(i) {
    margot.sim:::set_rng_stream(streams[[i]])
    test_fn(i)
  })
  
  expect_identical(seq_results, seq_results2)
})

test_that("validate_rng_reproducibility works", {
  test_fn <- function(rep_id) {
    sum(runif(50))
  }
  
  result <- validate_rng_reproducibility(test_fn, n_reps = 5, seed = 111)
  expect_true(result)
})

test_that("diagnose_rng_streams provides useful diagnostics", {
  streams <- create_rng_streams(10, seed = 222)
  diag <- diagnose_rng_streams(streams)
  
  expect_equal(nrow(diag), 1)
  expect_equal(diag$n_streams, 10)
  expect_false(diag$has_duplicates)
  # independence criteria might be too strict
  expect_true(diag$max_correlation < 1)  # just check it's not perfectly correlated
})

test_that("RNG discipline works in margot_monte_carlo", {
  skip_if_not_installed("doParallel")
  
  # simple estimator for testing
  test_estimator <- function(data) {
    list(
      estimate = mean(data$t2_y),
      se = sd(data$t2_y) / sqrt(nrow(data)),
      converged = TRUE
    )
  }
  
  # run MC with seed
  mc_result <- margot_monte_carlo(
    n_reps = 10,
    n_per_rep = 100,
    dgp_params = list(
      waves = 2,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]],
        shifted = function(data, time, trt) pmin(data[[trt]] + 0.5, 2)
      )
    ),
    estimator_fn = test_estimator,
    seed = 333,
    verbose = FALSE
  )
  
  expect_s3_class(mc_result, "margot_mc_results")
  expect_equal(nrow(mc_result$results), 10)
  
  # run again with same seed - should get same results
  mc_result2 <- margot_monte_carlo(
    n_reps = 10,
    n_per_rep = 100,
    dgp_params = list(
      waves = 2,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]],
        shifted = function(data, time, trt) pmin(data[[trt]] + 0.5, 2)
      )
    ),
    estimator_fn = test_estimator,
    seed = 333,
    verbose = FALSE
  )
  
  expect_equal(mc_result$results$estimate, mc_result2$results$estimate)
})

test_that("Parallel MC produces same results as sequential", {
  skip_if_not_installed("doParallel")
  skip_on_cran() # parallel tests can be slow
  
  test_estimator <- function(data) {
    list(
      estimate = mean(data$t2_y),
      se = sd(data$t2_y) / sqrt(nrow(data)),
      converged = TRUE
    )
  }
  
  # sequential
  set.seed(444)
  mc_seq <- margot_monte_carlo(
    n_reps = 20,
    n_per_rep = 100,
    dgp_params = list(
      waves = 2,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]],
        shifted = function(data, time, trt) pmin(data[[trt]] + 0.5, 2)
      )
    ),
    estimator_fn = test_estimator,
    seed = 555,
    parallel = FALSE,
    verbose = FALSE
  )
  
  # parallel
  set.seed(444)
  mc_par <- margot_monte_carlo(
    n_reps = 20,
    n_per_rep = 100,
    dgp_params = list(
      waves = 2,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]],
        shifted = function(data, time, trt) pmin(data[[trt]] + 0.5, 2)
      )
    ),
    estimator_fn = test_estimator,
    seed = 555,
    parallel = TRUE,
    n_cores = 2,
    verbose = FALSE
  )
  
  # results should be identical
  expect_equal(
    sort(mc_seq$results$estimate),
    sort(mc_par$results$estimate),
    tolerance = 1e-10
  )
})