library(margot.sim)

test_that("create_distribution creates valid distribution objects", {
  # parametric distribution
  dist_norm <- create_distribution("normal", params = list(mean = 5, sd = 2))
  expect_s3_class(dist_norm, "parametric_distribution")
  expect_equal(dist_norm$family, "normal")
  expect_equal(dist_norm$params$mean, 5)
  expect_equal(dist_norm$params$sd, 2)
  
  # binomial distribution
  dist_binom <- create_distribution("binomial", params = list(size = 1, prob = 0.7))
  expect_s3_class(dist_binom, "parametric_distribution")
  expect_equal(dist_binom$family, "binomial")
  expect_equal(dist_binom$params$prob, 0.7)
  
  # custom distribution
  custom_gen <- function(n, mean = 0) rnorm(n, mean = mean, sd = 1)
  dist_custom <- create_distribution(custom_gen, params = list(mean = 0))
  expect_s3_class(dist_custom, "custom_distribution")
  expect_equal(dist_custom$family, "custom")
  expect_true(is.function(dist_custom$generate_fn))
})

test_that("create_distribution handles various inputs", {
  # non-function strings are treated as parametric families
  dist_unknown <- create_distribution("unknown_family")
  expect_s3_class(dist_unknown, "parametric_distribution")
  expect_equal(dist_unknown$family, "unknown_family")
  
  # it gets default identity links
  expect_true(is.function(dist_unknown$link))
  expect_true(is.function(dist_unknown$inverse_link))
})

test_that("generate_from_dist generates correct samples", {
  set.seed(123)
  
  # normal distribution
  dist_norm <- create_distribution("normal", params = list(sd = 2))
  samples <- generate_from_dist(dist_norm, n = 1000)
  expect_length(samples, 1000)
  expect_true(abs(mean(samples) - 0) < 0.2)  # should be close to 0 (default)
  expect_true(abs(sd(samples) - 2) < 0.2)     # should be close to sd
  
  # binomial distribution
  dist_binom <- create_distribution("binomial", params = list(size = 1, prob = 0.3))
  samples_bin <- generate_from_dist(dist_binom, n = 1000)
  expect_true(all(samples_bin %in% c(0, 1)))
  expect_true(abs(mean(samples_bin) - 0.3) < 0.05)  # should be close to prob
  
  # with linear predictor
  lp <- rnorm(100, mean = 1)
  samples_lp <- generate_from_dist(dist_binom, n = 100, linear_predictor = lp)
  expect_length(samples_lp, 100)
  expect_true(all(samples_lp %in% c(0, 1)))
})

test_that("generate_from_dist handles custom distributions", {
  set.seed(456)
  
  # custom generator matches expected signature
  custom_gen <- function(n, params = list()) {
    shape <- params$shape %||% 2
    rgamma(n, shape = shape, rate = 1)
  }
  dist_custom <- create_distribution(custom_gen, params = list(shape = 3))
  
  samples <- generate_from_dist(dist_custom, n = 500)
  expect_length(samples, 500)
  expect_true(all(samples > 0))  # gamma is positive
  expect_true(mean(samples) > 2 && mean(samples) < 4)  # roughly shape/rate
})

test_that("create_distribution_set creates valid sets", {
  # using predefined distributions
  dist_set <- create_distribution_set(
    baseline = margot_distributions$normal,
    exposure = margot_distributions$binary,
    outcome = margot_distributions$gamma_skewed
  )
  
  expect_s3_class(dist_set, "margot_distribution_set")
  expect_equal(names(dist_set), c("baseline", "exposure", "outcome", "confounder", "error"))
  expect_s3_class(dist_set$baseline, "parametric_distribution")
  expect_s3_class(dist_set$exposure, "parametric_distribution")
  expect_equal(dist_set$baseline$family, "normal")
  expect_equal(dist_set$exposure$family, "binomial")
  
  # defaults should be normal
  expect_equal(dist_set$confounder$family, "normal")
  expect_equal(dist_set$error$family, "normal")
})

test_that("check_distribution performs distribution tests", {
  set.seed(789)
  
  # normal data
  normal_data <- rnorm(200)
  result_norm <- check_distribution(normal_data, "normal", plot = FALSE)
  expect_true(is.list(result_norm))
  expect_true("test" %in% names(result_norm))
  expect_true(result_norm$test$p.value > 0.05)  # should not reject normality
  
  # exponential data tested against normal
  exp_data <- rexp(200)
  result_exp <- check_distribution(exp_data, "normal", plot = FALSE)
  expect_true(result_exp$test$p.value < 0.05)  # should reject normality
  
  # exponential data tested correctly
  result_exp2 <- check_distribution(exp_data, "exponential", plot = FALSE)
  expect_true(result_exp2$test$p.value > 0.05)  # should not reject
})

test_that("margot_simulate_flex integrates with distributions", {
  set.seed(111)
  
  # create custom distribution set
  dists <- create_distribution_set(
    baseline = create_distribution("gamma", params = list(shape = 2, rate = 1)),
    exposure = margot_distributions$binary,
    outcome = create_distribution("normal", params = list(mean = 0, sd = 1))
  )
  
  # simulate with custom distributions
  sim_data <- margot_simulate_flex(
    n = 100,
    waves = 2,
    distributions = dists
  )
  
  expect_true(is.data.frame(sim_data))
  expect_equal(nrow(sim_data), 100)
  
  # without distributions, should use standard simulate
  sim_standard <- margot_simulate_flex(n = 50, waves = 1)
  expect_true(is.data.frame(sim_standard))
  expect_equal(nrow(sim_standard), 50)
})

test_that("margot_distributions contains expected distributions", {
  # check that predefined distributions exist
  expect_true(is.list(margot_distributions))
  
  expected_dists <- c("normal", "binary", "gamma_skewed", "lognormal", 
                     "t_heavy", "zero_inflated_normal")
  for (dist_name in expected_dists) {
    expect_true(dist_name %in% names(margot_distributions))
    expect_s3_class(margot_distributions[[dist_name]], "margot_distribution")
  }
  
  # test a few distributions
  expect_equal(margot_distributions$normal$family, "normal")
  expect_equal(margot_distributions$binary$family, "binomial")
  expect_equal(margot_distributions$gamma_skewed$family, "gamma")
})