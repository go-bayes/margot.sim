# tests for extended shadow types

test_that("truncation shadow works correctly", {
  # create simple test data
  set.seed(123)
  data <- data.frame(
    cholesterol = rnorm(100, mean = 200, sd = 50),
    bp = rnorm(100, mean = 120, sd = 20)
  )
  
  # test simple truncation
  shadow <- create_truncation_shadow(
    variables = "cholesterol",
    lower = 100,
    upper = 300,
    type = "simple"
  )
  
  result <- apply_shadow(data, shadow)
  
  # check that original values are preserved
  expect_true("cholesterol_true" %in% names(result))
  expect_equal(result$cholesterol_true, data$cholesterol)
  
  # check that values outside bounds are NA
  expect_true(all(is.na(result$cholesterol[data$cholesterol < 100])))
  expect_true(all(is.na(result$cholesterol[data$cholesterol > 300])))
  expect_true(all(!is.na(result$cholesterol[data$cholesterol >= 100 & data$cholesterol <= 300])))
  
  # check truncation indicator
  expect_true("cholesterol_truncated" %in% names(result))
  expect_equal(sum(result$cholesterol_truncated), sum(data$cholesterol < 100 | data$cholesterol > 300))
  
  # test boundary truncation
  shadow_boundary <- create_truncation_shadow(
    variables = "bp",
    lower = 80,
    upper = 180,
    type = "boundary"
  )
  
  result_boundary <- apply_shadow(data, shadow_boundary)
  
  # check that values pile up at boundaries
  expect_true(all(result_boundary$bp[data$bp < 80] == 80, na.rm = TRUE))
  expect_true(all(result_boundary$bp[data$bp > 180] == 180, na.rm = TRUE))
  
  # test with infinite bounds
  shadow_upper <- create_truncation_shadow(
    variables = "cholesterol",
    upper = 250,
    type = "simple"
  )
  
  result_upper <- apply_shadow(data, shadow_upper)
  expect_true(all(!is.na(result_upper$cholesterol[data$cholesterol <= 250])))
  expect_true(all(is.na(result_upper$cholesterol[data$cholesterol > 250])))
})

test_that("coarsening shadow works correctly", {
  # create test data
  set.seed(123)
  data <- data.frame(
    age = runif(100, 18, 80),
    income = rlnorm(100, log(50000), 0.5)
  )
  
  # test basic coarsening with midpoints
  shadow <- create_coarsening_shadow(
    variables = "age",
    breaks = c(0, 25, 35, 45, 55, 65, Inf),
    type = "midpoint"
  )
  
  result <- apply_shadow(data, shadow)
  
  # check that original values are preserved
  expect_true("age_true" %in% names(result))
  expect_equal(result$age_true, data$age)
  
  # check coarsening indicator
  expect_true("age_coarsened" %in% names(result))
  expect_true(all(result$age_coarsened))
  
  # check that values are reasonable midpoints
  # ages 25-35 should become 30
  in_range <- data$age >= 25 & data$age < 35
  expect_true(all(abs(result$age[in_range] - 30) < 1, na.rm = TRUE))
  
  # test coarsening with labels
  shadow_labels <- create_coarsening_shadow(
    variables = "age",
    breaks = c(0, 25, 35, 45, 55, 65, Inf),
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    type = "midpoint"
  )
  
  result_labels <- apply_shadow(data, shadow_labels)
  expect_true(is.factor(result_labels$age))
  expect_equal(levels(result_labels$age), c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  
  # test lower bound type
  shadow_lower <- create_coarsening_shadow(
    variables = "income",
    breaks = c(0, 25000, 50000, 75000, 100000, Inf),
    type = "lower"
  )
  
  result_lower <- apply_shadow(data, shadow_lower)
  
  # check that values are at lower bounds
  in_range_income <- data$income >= 25000 & data$income < 50000
  expect_true(all(result_lower$income[in_range_income] == 25000, na.rm = TRUE))
  
  # test heaping
  shadow_heap <- create_coarsening_shadow(
    variables = "age",
    breaks = 10,  # 10 equal bins
    type = "heaping",
    heaping_digits = c(0, 5),
    heaping_prob = 0.8
  )
  
  result_heap <- apply_shadow(data, shadow_heap)
  
  # check that many values end in 0 or 5
  last_digits <- result_heap$age %% 10
  heap_proportion <- mean(last_digits %in% c(0, 5), na.rm = TRUE)
  # with 80% heaping probability, we expect high proportion
  expect_true(heap_proportion > 0.5)
})

test_that("mode effects shadow works correctly", {
  # create test data with different survey modes
  set.seed(123)
  n <- 200
  data <- data.frame(
    income = rnorm(n, mean = 50000, sd = 20000),
    satisfaction = runif(n, 1, 10),
    survey_mode = sample(c("phone", "online", "in_person", "paper"), n, replace = TRUE)
  )
  
  # create mode effects shadow
  shadow <- create_mode_effects_shadow(
    variables = c("income", "satisfaction"),
    mode_var = "survey_mode",
    effect_specs = list(
      phone = list(shift = -5000, scale = 0.9),      # underreporting
      online = list(shift = 2000, scale = 1.05),     # slight overreporting
      paper = list(scale = 1, noise = 5000)          # more noise
    ),
    reference_mode = "in_person"
  )
  
  result <- apply_shadow(data, shadow)
  
  # check that original values are preserved
  expect_true("income_true" %in% names(result))
  expect_true("satisfaction_true" %in% names(result))
  
  # check mode effect indicators
  expect_true("income_mode_affected" %in% names(result))
  expect_true("satisfaction_mode_affected" %in% names(result))
  
  # check that reference mode is unaffected
  in_person_idx <- which(data$survey_mode == "in_person")
  expect_equal(result$income[in_person_idx], data$income[in_person_idx])
  expect_equal(result$satisfaction[in_person_idx], data$satisfaction[in_person_idx])
  
  # check phone effects (scale then shift)
  phone_idx <- which(data$survey_mode == "phone")
  expected_phone_income <- data$income[phone_idx] * 0.9 - 5000
  expect_equal(result$income[phone_idx], expected_phone_income)
  
  # check online effects
  online_idx <- which(data$survey_mode == "online")
  expected_online_income <- data$income[online_idx] * 1.05 + 2000
  expect_equal(result$income[online_idx], expected_online_income)
  
  # check paper effects (should have added noise)
  paper_idx <- which(data$survey_mode == "paper")
  # variance should be higher due to noise
  expect_true(var(result$income[paper_idx] - data$income[paper_idx]) > 1000)
})

test_that("shadow creation validates inputs correctly", {
  # truncation shadow validation
  expect_error(
    create_truncation_shadow(variables = character()),
    "Must specify at least one variable"
  )
  
  expect_error(
    create_truncation_shadow(variables = "x", lower = 10, upper = 5),
    "Lower bound must be less than upper bound"
  )
  
  expect_error(
    create_truncation_shadow(variables = "x", lower = "a"),
    "must be numeric"
  )
  
  # coarsening shadow validation
  expect_error(
    create_coarsening_shadow(variables = character(), breaks = 5),
    "Must specify at least one variable"
  )
  
  expect_error(
    create_coarsening_shadow(variables = "x", breaks = numeric()),
    "Must specify breaks"
  )
  
  expect_error(
    create_coarsening_shadow(
      variables = "x", 
      breaks = c(0, 10, 20),
      labels = c("Low", "Medium", "High")  # too many labels
    ),
    "Number of labels"
  )
  
  expect_error(
    create_coarsening_shadow(
      variables = "x",
      breaks = 5,
      type = "heaping",
      heaping_prob = 1.5
    ),
    "must be a probability"
  )
  
  # mode effects validation
  expect_error(
    create_mode_effects_shadow(
      variables = character(),
      mode_var = "mode",
      effect_specs = list()
    ),
    "Must specify at least one variable"
  )
  
  expect_error(
    create_mode_effects_shadow(
      variables = "x",
      mode_var = c("mode1", "mode2"),
      effect_specs = list()
    ),
    "must be a single character string"
  )
  
  expect_error(
    create_mode_effects_shadow(
      variables = "x",
      mode_var = "mode",
      effect_specs = list(
        phone = list(invalid_effect = 0.5)
      )
    ),
    "Invalid effect types"
  )
})

test_that("shadows handle missing data gracefully", {
  # create data with missing values
  data <- data.frame(
    x = c(1:5, NA, 7:10),
    y = c(NA, 2:10),
    mode = rep(c("A", "B"), 5)
  )
  
  # truncation with missing
  shadow_trunc <- create_truncation_shadow("x", lower = 3, upper = 8)
  result_trunc <- apply_shadow(data, shadow_trunc)
  expect_true(is.na(result_trunc$x[6]))  # original NA preserved
  expect_equal(sum(is.na(result_trunc$x)), 5)  # 1 original + 4 truncated (1,2,9,10)
  
  # coarsening with missing
  shadow_coarse <- create_coarsening_shadow("y", breaks = 3)
  result_coarse <- apply_shadow(data, shadow_coarse)
  expect_true(is.na(result_coarse$y[1]))  # original NA preserved
})

test_that("shadows work with multiple variables", {
  # create data
  set.seed(123)
  data <- data.frame(
    var1 = rnorm(50, 10, 2),
    var2 = rnorm(50, 20, 3),
    var3 = rnorm(50, 30, 4)
  )
  
  # apply truncation to multiple variables
  shadow <- create_truncation_shadow(
    variables = c("var1", "var2", "var3"),
    lower = 5,
    upper = 35,
    type = "boundary"
  )
  
  result <- apply_shadow(data, shadow)
  
  # check all variables were processed
  expect_true(all(c("var1_true", "var2_true", "var3_true") %in% names(result)))
  expect_true(all(c("var1_truncated", "var2_truncated", "var3_truncated") %in% names(result)))
  
  # check bounds applied correctly
  expect_true(all(result$var1 >= 5 & result$var1 <= 35))
  expect_true(all(result$var2 >= 5 & result$var2 <= 35))
  expect_true(all(result$var3 >= 5 & result$var3 <= 35))
})

test_that("heaping algorithm works realistically", {
  # test heaping with age data
  set.seed(123)
  data <- data.frame(
    age = runif(1000, 20, 70)
  )
  
  shadow <- create_coarsening_shadow(
    variables = "age",
    breaks = c(0, 30, 40, 50, 60, 100),
    type = "heaping",
    heaping_digits = c(0, 5),
    heaping_prob = 0.7
  )
  
  result <- apply_shadow(data, shadow)
  
  # check heaping pattern by age group
  # younger people (20-40) often report 25, 30, 35
  young <- result$age[data$age >= 20 & data$age < 40]
  young_heap <- sum(young %in% c(20, 25, 30, 35, 40)) / length(young)
  expect_true(young_heap > 0.3)  # substantial heaping
  
  # middle aged (40-60) often report 45, 50, 55
  middle <- result$age[data$age >= 40 & data$age < 60]
  middle_heap <- sum(middle %in% c(40, 45, 50, 55, 60)) / length(middle)
  expect_true(middle_heap > 0.3)  # substantial heaping
})