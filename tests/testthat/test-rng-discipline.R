# Test RNG discipline implementation

test_that("RNG streams are independent", {
  streams <- create_rng_streams(10, seed = 123)
  
  # check we have the right number of streams
  expect_equal(length(streams), 10)
  
  # check no duplicate streams
  expect_equal(anyDuplicated(streams), 0)
  
  # check streams produce different sequences
  samples <- lapply(streams, function(stream) {
    set_rng_stream(stream)
    runif(5)
  })
  
  # no two streams should produce identical sequences
  for (i in 1:9) {
    for (j in (i+1):10) {
      expect_false(identical(samples[[i]], samples[[j]]))
    }
  }
})

test_that("RNG streams are reproducible", {
  # create streams twice with same seed
  streams1 <- create_rng_streams(5, seed = 456)
  streams2 <- create_rng_streams(5, seed = 456)
  
  expect_identical(streams1, streams2)
  
  # check that using streams produces same results
  results1 <- lapply(streams1, function(stream) {
    set_rng_stream(stream)
    rnorm(10)
  })
  
  results2 <- lapply(streams2, function(stream) {
    set_rng_stream(stream)
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
    set_rng_stream(streams[[i]])
    test_fn(i)
  })
  
  # run in parallel
  cl <- parallel::makeCluster(2)
  on.exit(parallel::stopCluster(cl))
  
  parallel::clusterExport(cl, c("set_rng_stream"), envir = environment())
  
  par_results <- parallel::parLapply(cl, 1:20, function(i) {
    set_rng_stream(streams[[i]])
    test_fn(i)
  })
  
  expect_identical(seq_results, par_results)
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
  expect_true(diag$appears_independent)
  expect_lt(diag$max_correlation, 0.1)
})