# Test S3 methods for margot.sim
library(margot.sim)

# shadow constructor tests --------------------------------------------

test_that("new_shadow creates valid shadow objects", {
  # basic shadow creation
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  expect_s3_class(shadow, "margot_shadow")
  expect_s3_class(shadow, "measurement_error_shadow")
  expect_equal(shadow$type, "measurement_error")
  expect_equal(shadow$params$sigma, 0.5)
  
  # has metadata
  expect_true("metadata" %in% names(shadow))
  expect_true("created" %in% names(shadow$metadata))
  expect_true("version" %in% names(shadow$metadata))
})

test_that("new_shadow validates shadow types", {
  expect_error(
    new_shadow(type = "invalid_type", params = list()),
    "Invalid shadow type"
  )
})

test_that("shadow validation works for measurement error", {
  # missing required params
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list(error_type = "classical")
    ),
    "requires 'variables' parameter"
  )
  
  # invalid error type
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "invalid"
      )
    ),
    "Invalid error_type"
  )
  
  # missing sigma for classical error
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "classical"
      )
    ),
    "requires numeric sigma"
  )
  
  # misclassification validation
  expect_error(
    new_shadow(
      type = "measurement_error",
      params = list(
        variables = "x",
        error_type = "misclassification",
        sensitivity = 1.5
      )
    ),
    "Sensitivity and specificity must be between 0 and 1"
  )
})

test_that("shadow validation works for missing data", {
  # valid shadow
  shadow <- new_shadow(
    type = "missing_data",
    params = list(
      mechanism = "MCAR",
      prob = 0.2
    )
  )
  expect_s3_class(shadow, "missing_data_shadow")
  
  # invalid mechanism
  expect_error(
    new_shadow(
      type = "missing_data",
      params = list(mechanism = "invalid", prob = 0.2)
    ),
    "Invalid mechanism"
  )
  
  # invalid probability
  expect_error(
    new_shadow(
      type = "missing_data",
      params = list(mechanism = "MCAR", prob = 1.5)
    ),
    "between 0 and 1"
  )
})

test_that("shadow validation works for truncation", {
  # valid truncation
  shadow <- new_shadow(
    type = "truncation",
    params = list(lower = 0, upper = 10)
  )
  expect_s3_class(shadow, "truncation_shadow")
  
  # invalid bounds
  expect_error(
    new_shadow(
      type = "truncation",
      params = list(lower = 10, upper = 5)
    ),
    "lower bound must be less than upper"
  )
  
  # missing bounds
  expect_error(
    new_shadow(
      type = "truncation",
      params = list()
    ),
    "at least one of 'lower' or 'upper'"
  )
})

test_that("shadow print method works", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = c("x", "y", "z", "a", "b"),
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  output <- capture.output(print(shadow))
  expect_match(output[1], "<margot_shadow>")
  expect_match(output[2], "Type: measurement_error")
  expect_match(output[4], "variables: x, y, z, ...")  # should truncate
})

test_that("shadow summary method works", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  summary_obj <- summary(shadow)
  expect_s3_class(summary_obj, "summary.margot_shadow")
  expect_equal(summary_obj$type, "measurement_error")
  expect_equal(summary_obj$n_params, 3)
  expect_equal(summary_obj$param_names, c("variables", "error_type", "sigma"))
})

# scenario constructor tests ------------------------------------------

test_that("new_scenario creates valid scenario objects", {
  shadow1 <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  
  # scenario with single shadow
  scenario <- new_scenario(
    name = "Test Scenario",
    description = "A test scenario",
    shadows = shadow1
  )
  
  expect_s3_class(scenario, "margot_scenario")
  expect_equal(scenario$name, "Test Scenario")
  expect_equal(scenario$description, "A test scenario")
  expect_equal(length(scenario$shadows), 1)
  
  # scenario with multiple shadows
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(mechanism = "MCAR", prob = 0.1)
  )
  
  scenario2 <- new_scenario(
    name = "Multi Shadow",
    shadows = list(shadow1, shadow2)
  )
  
  expect_equal(length(scenario2$shadows), 2)
})

test_that("scenario validation works", {
  # invalid name
  expect_error(
    new_scenario(name = 123),
    "Scenario name must be a single character string"
  )
  
  # invalid shadows
  expect_error(
    new_scenario(name = "Test", shadows = "not a shadow"),
    "Shadows must be a list"
  )
  
  # non-shadow in list
  expect_error(
    new_scenario(name = "Test", shadows = list("not a shadow")),
    "is not a margot_shadow object"
  )
})

test_that("scenario print method works", {
  shadow1 <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(mechanism = "MCAR", prob = 0.1)
  )
  
  scenario <- new_scenario(
    name = "Test Scenario",
    description = "Testing print",
    shadows = list(shadow1, shadow2)
  )
  
  output <- capture.output(print(scenario))
  expect_match(output[1], "<margot_scenario>")
  expect_match(output[2], "Name: Test Scenario")
  expect_match(output[3], "Description: Testing print")
  expect_match(output[4], "Number of shadows: 2")
  expect_match(output[5], "measurement_error, missing_data")
})

test_that("scenario combination works", {
  shadow1 <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(mechanism = "MCAR", prob = 0.1)
  )
  
  scenario1 <- new_scenario(name = "S1", shadows = shadow1)
  scenario2 <- new_scenario(name = "S2", shadows = shadow2)
  
  combined <- c(scenario1, scenario2)
  
  expect_s3_class(combined, "margot_scenario")
  expect_equal(combined$name, "S1 + S2")
  expect_equal(length(combined$shadows), 2)
})

test_that("scenario subsetting works", {
  shadow1 <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(mechanism = "MCAR", prob = 0.1)
  )
  
  scenario <- new_scenario(
    name = "Test",
    shadows = list(shadow1, shadow2)
  )
  
  # extract single shadow
  single <- scenario[1]
  expect_s3_class(single, "margot_shadow")
  expect_equal(single$type, "measurement_error")
  
  # extract multiple shadows as new scenario
  subset <- scenario[1:2]
  expect_s3_class(subset, "margot_scenario")
  expect_equal(length(subset), 2)
})

test_that("scenario length and names work", {
  shadow1 <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(mechanism = "MCAR", prob = 0.1)
  )
  
  scenario <- new_scenario(
    name = "Test",
    shadows = list(shadow1, shadow2)
  )
  
  expect_equal(length(scenario), 2)
  expect_equal(names(scenario), c("measurement_error", "missing_data"))
})

# shadow list tests ---------------------------------------------------

test_that("shadow_list creation and methods work", {
  shadow1 <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(mechanism = "MCAR", prob = 0.1)
  )
  
  # create shadow list
  shadow_list <- new_shadow_list(shadow1, shadow2)
  
  expect_s3_class(shadow_list, "shadow_list")
  expect_equal(length(shadow_list), 2)
  
  # print method
  output <- capture.output(print(shadow_list))
  expect_match(output[1], "<shadow_list> with 2 shadows")
  
  # combining
  shadow3 <- new_shadow(
    type = "truncation",
    params = list(lower = 0, upper = 10)
  )
  
  combined <- c(shadow_list, shadow3)
  expect_equal(length(combined), 3)
  
  # subsetting
  subset <- shadow_list[1]
  expect_s3_class(subset, "margot_shadow")
})

# helper function tests -----------------------------------------------

test_that("is_shadow and is_scenario work", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  scenario <- new_scenario(name = "Test", shadows = shadow)
  
  expect_true(is_shadow(shadow))
  expect_false(is_shadow(scenario))
  expect_false(is_shadow("not a shadow"))
  
  expect_true(is_scenario(scenario))
  expect_false(is_scenario(shadow))
  expect_false(is_scenario("not a scenario"))
})

test_that("as_shadow conversions work", {
  # from shadow (identity)
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  expect_identical(as_shadow(shadow), shadow)
  
  # from list
  shadow_list <- list(
    type = "measurement_error",
    params = list(variables = "y", error_type = "classical", sigma = 1)
  )
  converted <- as_shadow(shadow_list)
  expect_s3_class(converted, "margot_shadow")
  expect_equal(converted$params$variables, "y")
  
  # invalid list
  expect_error(
    as_shadow(list(params = list())),
    "must have a 'type' element"
  )
})

test_that("as_scenario conversions work", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(variables = "x", error_type = "classical", sigma = 0.5)
  )
  
  # from shadow
  scenario <- as_scenario(shadow, name = "Converted")
  expect_s3_class(scenario, "margot_scenario")
  expect_equal(scenario$name, "Converted")
  expect_equal(length(scenario$shadows), 1)
  
  # from list that looks like scenario
  scenario_list <- list(
    name = "From List",
    description = "Test",
    shadows = list(shadow)
  )
  converted <- as_scenario(scenario_list)
  expect_equal(converted$name, "From List")
  expect_equal(converted$description, "Test")
})

# data frame conversion tests -----------------------------------------

test_that("as.data.frame methods work", {
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  # shadow to data frame
  df <- as.data.frame(shadow)
  expect_s3_class(df, "data.frame")
  expect_equal(df$type, "measurement_error")
  expect_equal(df$variables, "x")
  expect_equal(df$sigma, 0.5)
  
  # scenario to data frame
  shadow2 <- new_shadow(
    type = "missing_data",
    params = list(mechanism = "MCAR", prob = 0.1)
  )
  
  scenario <- new_scenario(
    name = "Test Scenario",
    shadows = list(shadow, shadow2)
  )
  
  scenario_df <- as.data.frame(scenario)
  expect_equal(nrow(scenario_df), 2)
  expect_equal(scenario_df$scenario[1], "Test Scenario")
  expect_equal(scenario_df$shadow_type[1], "measurement_error")
  expect_equal(scenario_df$shadow_type[2], "missing_data")
})

# edge cases ----------------------------------------------------------

test_that("empty scenarios handled correctly", {
  empty_scenario <- new_scenario(name = "Empty")
  
  expect_equal(length(empty_scenario), 0)
  expect_equal(names(empty_scenario), character(0))
  
  df <- as.data.frame(empty_scenario)
  expect_equal(nrow(df), 0)
})

test_that("special shadow types validate correctly", {
  # positivity shadow
  positivity <- new_shadow(
    type = "positivity",
    params = list(
      exposure_var = "treatment",
      filter_fn = function(data) data$x > 0
    )
  )
  expect_s3_class(positivity, "positivity_shadow")
  
  # coarsening shadow
  coarsening <- new_shadow(
    type = "coarsening",
    params = list(
      coarsen_fn = function(x) round(x)
    )
  )
  expect_s3_class(coarsening, "coarsening_shadow")
  
  # item missingness
  item_miss <- new_shadow(
    type = "item_missingness",
    params = list(
      variables = c("q1", "q2", "q3"),
      missing_rate = 0.15
    )
  )
  expect_s3_class(item_miss, "item_missingness_shadow")
})