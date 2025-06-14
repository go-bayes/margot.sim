# Test suite for scenario infrastructure
library(margot.sim)

test_that("create_scenario works correctly", {
  # basic scenario creation
  scenario <- create_scenario(
    name = "Test Scenario",
    shadows = list(),
    description = "A test scenario",
    justification = "For testing",
    references = c("Ref 1", "Ref 2")
  )
  
  expect_s3_class(scenario, "margot_scenario")
  expect_equal(scenario$name, "Test Scenario")
  expect_equal(length(scenario$shadows), 0)
  expect_equal(scenario$n_shadows, 0)
  expect_equal(length(scenario$references), 2)
  
  # scenario with shadows
  shadow1 <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "x",
      error_type = "classical",
      sigma = 0.1
    )
  )
  
  shadow2 <- create_item_missingness_shadow(
    variables = "y",
    missing_mechanism = "MCAR",
    missing_rate = 0.1
  )
  
  scenario_with_shadows <- create_scenario(
    name = "Complex Scenario",
    shadows = list(measurement = shadow1, missing = shadow2)
  )
  
  expect_equal(scenario_with_shadows$n_shadows, 2)
  expect_equal(names(scenario_with_shadows$shadows), c("measurement", "missing"))
  
  # error on invalid inputs
  expect_error(create_scenario(123), "Name must be a single character string")
  expect_error(create_scenario("Test", shadows = "not a list"), "Shadows must be a list")
})

test_that("scenario printing works", {
  scenario <- create_scenario(
    name = "Print Test",
    shadows = list(
      create_shadow(
        type = "measurement_error",
        params = list(
          variables = "x",
          error_type = "classical",
          sigma = 0.1
        )
      )
    ),
    description = "Test description",
    justification = "Test justification",
    references = c("Test ref")
  )
  
  output <- capture.output(print(scenario))
  expect_true(any(grepl("Print Test", output)))
  expect_true(any(grepl("Test description", output)))
  expect_true(any(grepl("Test justification", output)))
  expect_true(any(grepl("Test ref", output)))
  expect_true(any(grepl("measurement_error", output)))
})

test_that("apply_scenario works correctly", {
  # generate test data
  set.seed(123)
  test_data <- data.frame(
    id = 1:100,
    x = rnorm(100),
    y = rnorm(100)
  )
  
  # create scenario with measurement error
  scenario <- create_scenario(
    name = "Measurement Error",
    shadows = list(
      create_shadow(
        type = "measurement_error",
        params = list(
          variables = "x",
          error_type = "classical",
          sigma = 0.5
        )
      )
    )
  )
  
  # apply scenario
  result <- apply_scenario(test_data, scenario)
  
  expect_s3_class(result, "margot_scenario_result")
  expect_s3_class(result, "margot_shadow_result")
  expect_equal(result$scenario, "Measurement Error")
  expect_true("data_true" %in% names(result))
  expect_true("data_observed" %in% names(result))
  expect_true("x_true" %in% names(result$data_observed))
  
  # verify measurement error was applied
  expect_true(sd(result$data_observed$x - result$data_observed$x_true) > 0.3)
  
  # test with empty scenario (oracle)
  oracle_scenario <- create_scenario(name = "Oracle", shadows = list())
  oracle_result <- apply_scenario(test_data, oracle_scenario)
  
  # with no shadows, data should be the same but attributes may differ
  expect_equal(oracle_result$data_true[names(test_data)], oracle_result$data_observed[names(test_data)])
})

test_that("scenario comparison works", {
  # generate test data
  set.seed(456)
  sim_data <- margot_simulate(n = 200, waves = 2)
  
  # create scenarios
  scenarios <- list(
    oracle = create_scenario("Oracle", shadows = list()),
    measurement = create_scenario(
      "Measurement Error",
      shadows = list(
        create_shadow(
          type = "measurement_error",
          params = list(
            variables = "t1_a",
            error_type = "misclassification",
            sensitivity = 0.85,
            specificity = 0.90
          )
        )
      )
    ),
    missing = create_scenario(
      "Missing Data",
      shadows = list(
        create_item_missingness_shadow(
          variables = "t2_y",
          missing_mechanism = "MAR",
          missing_rate = 0.2,
          dependent_vars = "b1"
        )
      )
    )
  )
  
  # compare scenarios
  comparison <- compare_scenarios(
    sim_data,
    scenarios,
    exposure = "t1_a",
    outcome = "t2_y",
    estimands = c("ate", "att")
  )
  
  expect_s3_class(comparison, "margot_scenario_comparison")
  expect_equal(comparison$n_scenarios, 3)
  expect_equal(comparison$scenario_names, c("oracle", "measurement", "missing"))
  expect_equal(comparison$estimands, c("ate", "att"))
  
  # check results structure
  expect_true(all(c("oracle", "measurement", "missing") %in% names(comparison$results)))
  expect_true("effects" %in% names(comparison$results$oracle))
  expect_true("ate" %in% names(comparison$results$oracle$effects))
  
  # oracle should have no shadows
  expect_equal(comparison$results$oracle$n_shadows, 0)
  expect_equal(comparison$results$measurement$n_shadows, 1)
  expect_equal(comparison$results$missing$n_shadows, 1)
  
  # test printing
  output <- capture.output(print(comparison))
  expect_true(any(grepl("Scenario Comparison Results", output)))
  expect_true(any(grepl("ATE", output)))
  expect_true(any(grepl("ATT", output)))
})

test_that("scenario library functions work", {
  # test oracle scenario
  oracle <- scenario_oracle()
  expect_s3_class(oracle, "margot_scenario")
  expect_equal(oracle$name, "Oracle")
  expect_equal(length(oracle$shadows), 0)
  
  # test typical RCT scenario
  rct <- scenario_rct_typical()
  expect_s3_class(rct, "margot_scenario")
  expect_true(length(rct$shadows) > 0)
  expect_true("measurement" %in% names(rct$shadows))
  expect_true("dropout" %in% names(rct$shadows))
  
  # test custom parameters
  rct_custom <- scenario_rct_typical(
    measurement_error_sd = 0.2,
    dropout_rate = 0.25,
    name = "Custom RCT"
  )
  expect_equal(rct_custom$name, "Custom RCT")
  expect_equal(rct_custom$shadows$measurement$params$sigma, 0.2)
  expect_equal(rct_custom$shadows$dropout$params$rate, 0.25)
  
  # test EHR scenario
  ehr <- scenario_ehr_typical()
  expect_s3_class(ehr, "margot_scenario")
  expect_true("diagnosis_misclass" %in% names(ehr$shadows))
  expect_true("missing_labs" %in% names(ehr$shadows))
  
  # test survey scenario
  survey <- scenario_survey_typical()
  expect_s3_class(survey, "margot_scenario")
  expect_true("selfreport_error" %in% names(survey$shadows))
  
  # test pessimistic scenario
  pessimistic <- scenario_pessimistic()
  expect_s3_class(pessimistic, "margot_scenario")
  expect_true(length(pessimistic$shadows) >= 5) # should have many shadows
})

test_that("scenario collection works", {
  # get default collection
  collection <- scenario_collection()
  
  expect_type(collection, "list")
  expect_true("oracle" %in% names(collection))
  expect_true("rct_typical" %in% names(collection))
  expect_true("ehr" %in% names(collection))
  expect_true("pessimistic" %in% names(collection))
  
  # test selective inclusion
  subset_collection <- scenario_collection(include = c("oracle", "rct"))
  expect_equal(length(subset_collection), 3) # oracle, rct_typical, rct_pragmatic
  expect_true("oracle" %in% names(subset_collection))
  expect_true("rct_typical" %in% names(subset_collection))
  expect_false("ehr" %in% names(subset_collection))
})

test_that("scenario_from_template works", {
  # create from RCT template
  custom_rct <- scenario_from_template(
    "rct_typical",
    modifications = list(dropout_rate = 0.30),
    name = "High Dropout RCT"
  )
  
  expect_s3_class(custom_rct, "margot_scenario")
  expect_equal(custom_rct$name, "High Dropout RCT")
  
  # error on unknown template
  expect_error(
    scenario_from_template("unknown_template"),
    "Unknown template"
  )
})

test_that("sensitivity analysis works", {
  # use fixed data for reproducibility
  set.seed(789)
  fixed_data <- margot_simulate(n = 100, waves = 2)
  
  # create scenarios
  scenarios <- list(
    oracle = scenario_oracle(),
    realistic = scenario_rct_simple()
  )
  
  # run sensitivity analysis
  sens_result <- sensitivity_analysis(
    data_generator = fixed_data,
    scenarios = scenarios,
    intervention = list(t1_a = 1),
    outcome_var = "t2_y",
    n_sim = 10,
    parallel = FALSE
  )
  
  expect_s3_class(sens_result, "margot_sensitivity_analysis")
  expect_equal(names(sens_result$results), c("oracle", "realistic"))
  expect_equal(sens_result$n_sim, 10)
  
  # test summary
  sens_summary <- summary(sens_result)
  expect_s3_class(sens_summary, "data.frame")
  expect_true("Scenario" %in% names(sens_summary))
  expect_true("Mean_ATE" %in% names(sens_summary))
  expect_true("SD_ATE" %in% names(sens_summary))
  
  # test with function generator
  data_gen_fn <- function() {
    margot_simulate(n = 50, waves = 2)
  }
  
  sens_result_fn <- sensitivity_analysis(
    data_generator = data_gen_fn,
    scenarios = list(oracle = scenario_oracle()),
    intervention = list(t1_a = 1),
    outcome_var = "t2_y",
    n_sim = 5,
    parallel = FALSE
  )
  
  expect_s3_class(sens_result_fn, "margot_sensitivity_analysis")
})

test_that("scenario plotting works", {
  skip_if_not_installed("ggplot2")
  
  # create test comparison
  set.seed(321)
  sim_data <- margot_simulate(n = 100, waves = 2)
  
  scenarios <- list(
    oracle = scenario_oracle(),
    realistic = scenario_rct_simple(),
    pessimistic = scenario_pessimistic()
  )
  
  comparison <- compare_scenarios(
    sim_data,
    scenarios,
    exposure = "t1_a",
    outcome = "t2_y"
  )
  
  # test plot creation
  p <- plot(comparison)
  expect_s3_class(p, "ggplot")
  
  # test with different estimand
  p_att <- plot(comparison, estimand = "att")
  expect_s3_class(p_att, "ggplot")
})

test_that("scenarios handle edge cases correctly", {
  # empty data
  empty_data <- data.frame(x = numeric(0), y = numeric(0))
  scenario <- scenario_oracle()
  
  # applying scenario to empty data should work but return empty data
  result <- apply_scenario(empty_data, scenario)
  expect_equal(nrow(result$data_observed), 0)
  
  # missing variables
  test_data <- data.frame(x = 1:10)
  scenario_missing_var <- create_scenario(
    "Missing Var",
    shadows = list(
      create_shadow(
        type = "measurement_error",
        params = list(
          variables = "nonexistent",
          error_type = "classical",
          sigma = 0.1
        )
      )
    )
  )
  
  expect_error(apply_scenario(test_data, scenario_missing_var))
  
  # invalid scenario object
  expect_error(apply_scenario(test_data, list(name = "Not a scenario")))
})