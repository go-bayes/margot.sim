# Tests for S3 object system

test_that("shadow S3 constructor works", {
  # create a basic shadow
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = c("x", "y"),
      error_type = "classical",
      sigma = 0.5
    ),
    name = "test_shadow"
  )
  
  expect_s3_class(shadow, "margot_shadow")
  expect_s3_class(shadow, "measurement_error_shadow")
  expect_equal(shadow$type, "measurement_error")
  expect_equal(shadow$name, "test_shadow")
})

test_that("shadow validation works", {
  # valid shadow should pass
  shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = c("x", "y"),
      error_type = "classical",
      sigma = 0.5
    )
  )
  expect_silent(validate_shadow(shadow))
  
  # missing required field should fail
  bad_shadow <- new_shadow(
    type = "measurement_error",
    params = list(
      error_type = "classical",
      sigma = 0.5
    )
  )
  expect_error(validate_shadow(bad_shadow), "requires 'variables'")
  
  # invalid error type should fail
  bad_shadow2 <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "invalid_type"
    )
  )
  expect_error(validate_shadow(bad_shadow2), "Invalid error_type")
  
  # misclassification with invalid sensitivity
  bad_shadow3 <- new_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "misclassification",
      sensitivity = 1.5,
      specificity = 0.9
    )
  )
  expect_error(validate_shadow(bad_shadow3), "between 0 and 1")
})

test_that("create_shadow uses S3 system", {
  # should create and validate
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "test_var",
      error_type = "classical",
      sigma = 1
    )
  )
  
  expect_s3_class(shadow, "margot_shadow")
  expect_s3_class(shadow, "measurement_error_shadow")
})

test_that("scenario S3 constructor works", {
  # create shadows first
  shadow1 <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  shadow2 <- create_shadow(
    type = "censoring",
    params = list(rate = 0.2)
  )
  
  # create scenario
  scenario <- new_scenario(
    name = "Test Scenario",
    shadows = list(error = shadow1, censor = shadow2),
    description = "A test scenario",
    justification = "For testing"
  )
  
  expect_s3_class(scenario, "margot_scenario")
  expect_equal(scenario$name, "Test Scenario")
  expect_equal(scenario$n_shadows, 2)
})

test_that("scenario validation works", {
  # valid scenario should pass
  shadow <- create_shadow(
    type = "censoring",
    params = list(rate = 0.1)
  )
  
  scenario <- new_scenario(
    name = "Valid",
    shadows = list(shadow)
  )
  expect_silent(validate_scenario(scenario))
  
  # invalid shadow in scenario should fail
  scenario_bad <- list(
    name = "Bad",
    shadows = list("not a shadow"),
    description = "",
    justification = "",
    n_shadows = 1
  )
  class(scenario_bad) <- "margot_scenario"
  
  expect_error(validate_scenario(scenario_bad), "must be margot_shadow")
})

test_that("is_shadow and is_scenario work", {
  shadow <- create_shadow("censoring", list(rate = 0.1))
  scenario <- create_scenario("Test", list(shadow))
  
  expect_true(is_shadow(shadow))
  expect_false(is_shadow(scenario))
  expect_false(is_shadow(list()))
  
  expect_true(is_scenario(scenario))
  expect_false(is_scenario(shadow))
  expect_false(is_scenario(list()))
})

test_that("print methods work", {
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = c("x", "y"),
      error_type = "classical",
      sigma = 0.5
    ),
    name = "My Error Shadow"
  )
  
  expect_output(print(shadow), "margot Shadow Object")
  expect_output(print(shadow), "Type: measurement_error")
  expect_output(print(shadow), "Name: My Error Shadow")
})

test_that("shadow collections work", {
  shadow1 <- create_shadow("censoring", list(rate = 0.1))
  shadow2 <- create_shadow("censoring", list(rate = 0.2))
  
  # combine shadows
  shadows <- c(shadow1, shadow2)
  
  expect_s3_class(shadows, "shadow_list")
  expect_length(shadows, 2)
  
  # extraction should return single shadow
  extracted <- shadows[1]
  expect_s3_class(extracted, "margot_shadow")
})

test_that("as.data.frame methods work", {
  # shadow to data frame
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = c("x", "y"),
      error_type = "classical",
      sigma = 0.5
    ),
    name = "error_shadow"
  )
  
  df <- as.data.frame(shadow)
  expect_equal(nrow(df), 1)
  expect_equal(df$name, "error_shadow")
  expect_equal(df$type, "measurement_error")
  expect_equal(df$error_type, "classical")
  expect_equal(df$sigma, 0.5)
  
  # scenario to data frame
  scenario <- create_scenario(
    name = "Test Scenario",
    shadows = list(shadow),
    description = "A test scenario for conversion"
  )
  
  df_scenario <- as.data.frame(scenario)
  expect_equal(nrow(df_scenario), 1)
  expect_equal(df_scenario$name, "Test Scenario")
  expect_equal(df_scenario$n_shadows, 1)
})

test_that("scenario subsetting works", {
  shadow1 <- create_shadow("censoring", list(rate = 0.1), name = "censor1")
  shadow2 <- create_shadow("censoring", list(rate = 0.2), name = "censor2")
  
  scenario <- create_scenario(
    "Test",
    shadows = list(first = shadow1, second = shadow2)
  )
  
  # length method
  expect_equal(length(scenario), 2)
  
  # names method
  expect_equal(names(scenario), c("first", "second"))
  
  # subset by position
  sub1 <- scenario[1]
  expect_length(sub1, 1)
  
  # subset by name
  sub2 <- scenario["first"]
  expect_length(sub2, 1)
})

test_that("old interface compatibility", {
  # test as_shadow conversion
  old_shadow <- list(
    type = "censoring",
    params = list(rate = 0.15),
    name = "old_style_shadow"
  )
  
  new_shadow <- as_shadow(old_shadow)
  expect_s3_class(new_shadow, "margot_shadow")
  expect_s3_class(new_shadow, "censoring_shadow")
  
  # test as_scenario conversion
  old_scenario <- list(
    name = "Old Style",
    shadows = list(old_shadow),
    description = "Test",
    justification = "Testing conversion"
  )
  
  new_scenario <- as_scenario(old_scenario)
  expect_s3_class(new_scenario, "margot_scenario")
  expect_s3_class(new_scenario$shadows[[1]], "margot_shadow")
})