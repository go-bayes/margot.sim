# tests for multi-treatment support in margot_simulate_causal

test_that("single treatment works (backward compatibility)", {
  # simple interventions
  interventions <- list(
    never = function(data, time, trt) rep(0, nrow(data)),
    always = function(data, time, trt) rep(1, nrow(data))
  )
  
  # should work with single treatment
  result <- margot_simulate_causal(
    n = 100,
    waves = 2,
    treatments = "a",
    interventions = interventions,
    common_params = list(
      params = list(a_lag_y_coef = 0.5)
    )
  )
  
  expect_s3_class(result, "margot_causal_sim")
  expect_equal(result$metadata$treatments, "a")
  expect_equal(length(result$data), 2)  # two interventions
})

test_that("multiple treatments with vector specification works", {
  # create multi-treatment interventions
  interventions <- list(
    both_high = create_interaction_patterns(
      trt_patterns = list("a" = 1, "b" = 1),
      combination = "all"
    ),
    only_a = create_interaction_patterns(
      trt_patterns = list("a" = 1, "b" = 0),
      combination = "all"
    )
  )
  
  # simulate with two treatments
  result <- margot_simulate_causal(
    n = 100,
    waves = 2,
    treatments = c("a", "b"),
    interventions = interventions
  )
  
  expect_s3_class(result, "margot_causal_sim")
  expect_equal(length(result$metadata$treatments), 2)
  expect_true("a" %in% result$metadata$treatments)
  expect_true("b" %in% result$metadata$treatments)
})

test_that("treatments as named list works", {
  # test named list format
  interventions <- list(
    natural = function(data, time, trt) data[[trt]]
  )
  
  result <- margot_simulate_causal(
    n = 100,
    waves = 2,
    treatments = list(
      belief_god = "believe_god",
      belief_spirit = "believe_spirit"
    ),
    interventions = interventions
  )
  
  expect_s3_class(result, "margot_causal_sim")
  expect_equal(result$metadata$treatment_names, c("belief_god", "belief_spirit"))
  expect_equal(result$metadata$treatments, c("believe_god", "believe_spirit"))
})

test_that("LMTP-style interventions work with multi-treatment", {
  # create LMTP-style intervention
  lmtp_intervention <- function(data, trt_list) {
    all_trts <- unlist(trt_list)
    shifted <- list()
    
    for (trt in all_trts) {
      # shift all treatments to 1
      shifted[[trt]] <- ifelse(!is.na(data[[trt]]), 1, NA)
    }
    
    shifted
  }
  
  interventions <- list(
    all_high = lmtp_intervention
  )
  
  # should handle LMTP-style intervention
  result <- margot_simulate_causal(
    n = 100,
    waves = 2,
    treatments = c("a", "b"),
    interventions = interventions
  )
  
  expect_s3_class(result, "margot_causal_sim")
})

test_that("interaction patterns create correct shifts", {
  # test specific interaction patterns
  both_high <- create_interaction_patterns(
    trt_patterns = list("a" = 1, "b" = 1),
    combination = "all"
  )
  
  only_a <- create_interaction_patterns(
    trt_patterns = list("a" = 1, "b" = 0),
    combination = "all"
  )
  
  # create test data
  test_data <- data.frame(
    t1_a = c(0, 1, 0, 1),
    t1_b = c(0, 0, 1, 1)
  )
  
  # test both_high
  result_both <- both_high(test_data, list("t1_a", "t1_b"))
  expect_equal(result_both$t1_a, c(1, 1, 1, 1))
  expect_equal(result_both$t1_b, c(1, 1, 1, 1))
  
  # test only_a
  result_a <- only_a(test_data, list("t1_a", "t1_b"))
  expect_equal(result_a$t1_a, c(1, 1, 1, 1))
  expect_equal(result_a$t1_b, c(0, 0, 0, 0))
})

test_that("error handling for invalid treatments", {
  interventions <- list(
    natural = function(data, time, trt) data[[trt]]
  )
  
  # should error on non-character/list treatments
  expect_error(
    margot_simulate_causal(
      n = 100,
      waves = 2,
      treatments = 123,
      interventions = interventions
    ),
    "treatments must be a character vector or named list"
  )
})

test_that("wrapper correctly handles single-treatment interventions with multi-treatment", {
  # single-treatment intervention used with multiple treatments
  single_trt_intervention <- function(data, time, trt) {
    # only works on single treatment
    ifelse(data[[trt]] > 0.5, 1, 0)
  }
  
  interventions <- list(
    threshold = single_trt_intervention
  )
  
  # should still work by applying to first treatment
  result <- margot_simulate_causal(
    n = 100,
    waves = 2,
    treatments = c("a", "b"),
    interventions = interventions
  )
  
  expect_s3_class(result, "margot_causal_sim")
})