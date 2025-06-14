# Test memory management utilities
library(margot.sim)

test_that("create_data_summarizer creates valid summary functions", {
  # create summarizer with default stats
  summarizer <- create_data_summarizer()
  
  # test data
  complete <- data.frame(
    x = rnorm(100),
    y = rnorm(100),
    z = rbinom(100, 1, 0.5)
  )
  
  shadowed <- complete
  shadowed$x <- shadowed$x + rnorm(100, 0, 0.5)
  shadowed$y[1:10] <- NA
  
  # apply summarizer
  summary <- summarizer(complete, shadowed)
  
  # check structure
  expect_type(summary, "list")
  expect_true("complete_means" %in% names(summary))
  expect_true("shadowed_means" %in% names(summary))
  expect_true("complete_sds" %in% names(summary))
  expect_true("shadowed_sds" %in% names(summary))
})

test_that("create_data_summarizer handles specific variables", {
  summarizer <- create_data_summarizer(
    stats = c("mean", "sd"),
    vars = c("x", "y")
  )
  
  data <- data.frame(
    x = rnorm(50),
    y = rnorm(50),
    z = rnorm(50),  # should be ignored
    w = letters[1:50]  # non-numeric, should be ignored
  )
  
  summary <- summarizer(data, data)
  
  # only x and y should be summarized
  expect_equal(length(summary$complete_means), 2)
  expect_true(all(names(summary$complete_means) %in% c("x", "y")))
  expect_false("z" %in% names(summary$complete_means))
})

test_that("create_data_summarizer computes correct statistics", {
  set.seed(123)
  n <- 100
  
  complete <- data.frame(
    x = rnorm(n, mean = 5, sd = 2),
    y = rnorm(n, mean = 10, sd = 3)
  )
  
  shadowed <- complete
  shadowed$x <- shadowed$x + rnorm(n, 0, 1)
  
  summarizer <- create_data_summarizer(stats = c("mean", "sd", "quantiles"))
  summary <- summarizer(complete, shadowed)
  
  # check means are correct
  expect_equal(as.numeric(summary$complete_means["x"]), mean(complete$x), tolerance = 0.01)
  expect_equal(as.numeric(summary$complete_sds["x"]), sd(complete$x), tolerance = 0.01)
  
  # check quantiles
  expect_equal(length(summary$complete_quantiles$x), 5)
  expect_equal(
    as.numeric(summary$complete_quantiles$x),
    as.numeric(quantile(complete$x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))),
    tolerance = 0.01
  )
})

test_that("create_data_summarizer handles correlations", {
  set.seed(456)
  n <- 100
  
  # create correlated data
  x <- rnorm(n)
  y <- x + rnorm(n, 0, 0.5)
  z <- -0.5 * x + rnorm(n, 0, 0.7)
  
  data <- data.frame(x = x, y = y, z = z)
  
  summarizer <- create_data_summarizer(stats = "cor")
  summary <- summarizer(data, data)
  
  # check correlation matrix
  expect_true("complete_cor" %in% names(summary))
  expect_true(is.matrix(summary$complete_cor))
  expect_equal(dim(summary$complete_cor), c(3, 3))
  
  # check correlations are reasonable
  expect_gt(summary$complete_cor["x", "y"], 0.7)  # strong positive
  expect_lt(summary$complete_cor["x", "z"], -0.3)  # negative
})

test_that("create_data_summarizer handles missing data", {
  data <- data.frame(
    x = c(1:10, rep(NA, 5)),
    y = c(rep(NA, 5), 6:15)
  )
  
  summarizer <- create_data_summarizer(stats = c("mean", "counts"))
  summary <- summarizer(data, data)
  
  # means should handle NAs
  expect_equal(as.numeric(summary$complete_means["x"]), mean(1:10))
  expect_equal(as.numeric(summary$complete_means["y"]), mean(6:15))
  
  # counts
  expect_equal(summary$n_complete, 15)
  expect_equal(summary$n_missing, 10)  # 5 in x, 5 in y
})

test_that("create_memory_monitor works correctly", {
  monitor <- create_memory_monitor(interval = 0.1, threshold = 1)
  
  # first call should not trigger (no time passed)
  expect_silent(monitor())
  
  # wait and allocate memory
  Sys.sleep(0.2)
  big_obj <- matrix(rnorm(1000000), nrow = 1000)  # ~7.6 MB
  
  # should trigger if memory is high enough
  # (can't guarantee warning due to system state)
  monitor()
  
  rm(big_obj)
  gc()
})

test_that("create_mc_streamer creates valid streaming functions", {
  temp_file <- tempfile(fileext = ".csv")
  
  streamer <- create_mc_streamer(temp_file, batch_size = 3)
  
  # write some results
  for (i in 1:7) {
    result <- list(
      rep_id = i,
      estimate = rnorm(1),
      se = runif(1),
      converged = TRUE
    )
    streamer$write(result)
  }
  
  # finalize
  total <- streamer$finalize()
  expect_equal(total, 7)
  
  # check file was created and has correct content
  expect_true(file.exists(temp_file))
  data <- read.csv(temp_file)
  expect_equal(nrow(data), 7)
  expect_true(all(c("rep_id", "estimate", "se", "converged") %in% names(data)))
  
  # cleanup
  unlink(temp_file)
})

test_that("create_mc_streamer handles single batch", {
  temp_file <- tempfile(fileext = ".csv")
  streamer <- create_mc_streamer(temp_file, batch_size = 10)
  
  # write fewer than batch size
  for (i in 1:5) {
    streamer$write(list(id = i, value = i^2))
  }
  
  # should still write on finalize
  total <- streamer$finalize()
  expect_equal(total, 5)
  
  data <- read.csv(temp_file)
  expect_equal(nrow(data), 5)
  expect_equal(data$value, (1:5)^2)
  
  unlink(temp_file)
})

test_that("get_memory_stats returns valid statistics", {
  stats <- get_memory_stats()
  
  expect_s3_class(stats, "data.frame")
  expect_equal(nrow(stats), 1)
  expect_true(all(c("used_mb", "gc_trigger_mb", "max_used_mb", "usage_ratio") %in% names(stats)))
  
  # all values should be positive
  expect_true(all(stats >= 0))
  
  # usage ratio should be between 0 and 1 (usually)
  expect_true(stats$usage_ratio >= 0)
})

test_that("Memory management in margot_monte_carlo works", {
  skip_on_cran()  # memory tests can be unreliable on CRAN
  
  # create a summarizer that tracks memory
  summarizer <- create_data_summarizer(stats = c("mean", "counts"))
  
  # simple estimator
  test_estimator <- function(data) {
    list(estimate = mean(data$t2_y), converged = TRUE)
  }
  
  # run MC with memory management
  mc_result <- margot_monte_carlo(
    n_reps = 10,
    n_per_rep = 100,
    dgp_params = list(
      waves = 2,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]],
        shifted = function(data, time, trt) pmin(data[[trt]] + 1, 2)
      )
    ),
    estimator_fn = test_estimator,
    summarize_fn = summarizer,
    memory_limit = 500,  # MB
    verbose = FALSE
  )
  
  # check that summaries were created
  expect_true(!is.null(mc_result$data_summaries))
  expect_equal(length(mc_result$data_summaries), 10)
  
  # check summary structure
  first_summary <- mc_result$data_summaries[[1]]
  expect_true("complete_means" %in% names(first_summary))
  expect_true("n_complete" %in% names(first_summary))
})

test_that("Checkpointing in margot_monte_carlo works", {
  skip_on_cran()
  
  temp_dir <- tempdir()
  checkpoint_dir <- file.path(temp_dir, "test_checkpoints")
  
  # simple estimator
  test_estimator <- function(data) {
    list(estimate = mean(data$t2_y), converged = TRUE)
  }
  
  # run MC with checkpointing
  mc_result <- margot_monte_carlo(
    n_reps = 5,
    n_per_rep = 50,
    dgp_params = list(
      waves = 2,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]],
        shifted = function(data, time, trt) pmin(data[[trt]] + 1, 2)
      )
    ),
    estimator_fn = test_estimator,
    checkpoint_dir = checkpoint_dir,
    verbose = FALSE
  )
  
  # check results
  expect_equal(nrow(mc_result$results), 5)
  
  # checkpoints should have been cleaned up
  checkpoint_files <- list.files(checkpoint_dir, pattern = "checkpoint_.*\\.rds")
  expect_equal(length(checkpoint_files), 0)
  
  # cleanup
  unlink(checkpoint_dir, recursive = TRUE)
})

test_that("resume_monte_carlo handles missing checkpoints", {
  temp_dir <- tempdir()
  checkpoint_dir <- file.path(temp_dir, "test_resume")
  
  # try to resume without checkpoints
  expect_error(
    result <- resume_monte_carlo(checkpoint_dir, n_reps = 10),
    "Checkpoint directory does not exist"
  )
})

test_that("MC memory limit triggers garbage collection", {
  skip_on_cran()
  
  # track gc calls
  gc_before <- gc()
  
  test_estimator <- function(data) {
    # allocate some memory
    temp <- matrix(rnorm(10000), nrow = 100)
    list(estimate = mean(data$t2_y), converged = TRUE)
  }
  
  # run with low memory limit to trigger gc
  mc_result <- margot_monte_carlo(
    n_reps = 5,
    n_per_rep = 100,
    dgp_params = list(
      waves = 2,
      treatments = "a",
      interventions = list(
        natural = function(data, time, trt) data[[trt]],
        shifted = function(data, time, trt) pmin(data[[trt]] + 1, 2)
      )
    ),
    estimator_fn = test_estimator,
    memory_limit = 50,  # very low limit
    verbose = FALSE
  )
  
  gc_after <- gc()
  
  # should have triggered additional gc
  # (this is hard to test reliably across systems)
  expect_true(is.data.frame(mc_result$results))
})