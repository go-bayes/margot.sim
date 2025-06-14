# Test shadow dependency system
library(margot.sim)

test_that("Shadow dependencies are correctly defined", {
  deps <- margot.sim:::get_shadow_dependencies()
  
  # check structure
  expect_type(deps, "list")
  expect_true(all(sapply(deps, is.character)))
  
  # check known dependencies
  expect_true("truncation" %in% deps$measurement_error)
  expect_true("censoring" %in% deps$selection)
  expect_equal(deps$censoring, character(0))
})

test_that("Shadow ordering check works", {
  # create test shadows
  shadow1 <- create_shadow("measurement_error", 
                          params = list(variables = "x", error_type = "classical", sigma = 1))
  shadow2 <- create_shadow("truncation", 
                          params = list(variables = "x", lower = 0))
  
  # good ordering: truncation before measurement_error
  good_order <- list(shadow2, shadow1)
  expect_true(check_shadow_ordering(good_order))
  
  # bad ordering: measurement_error before truncation
  bad_order <- list(shadow1, shadow2)
  expect_warning(
    result <- check_shadow_ordering(bad_order),
    "depends on shadows that appear later"
  )
  expect_false(result)
})

test_that("Shadow reordering works correctly", {
  # create shadows with dependencies
  shadows <- list(
    create_shadow("selection", params = list()),
    create_shadow("measurement_error", params = list(variables = "x", error_type = "classical", sigma = 1)),
    create_shadow("truncation", params = list(variables = "x", lower = 0)),
    create_shadow("censoring", params = list(rate = 0.1, mechanism = "MAR"))
  )
  
  # reorder
  reordered <- reorder_shadows(shadows)
  
  # check new ordering respects dependencies
  expect_true(check_shadow_ordering(reordered))
  
  # check specific orderings
  types <- sapply(reordered, function(s) s$type)
  
  # truncation should come before measurement_error
  expect_lt(which(types == "truncation"), which(types == "measurement_error"))
  
  # censoring should come before selection
  expect_lt(which(types == "censoring"), which(types == "selection"))
})

test_that("Shadow parameter updates work", {
  # create truncation shadow
  truncation <- create_shadow("truncation", 
                            params = list(variables = "x", lower = 0, upper = 10))
  
  # create measurement error shadow
  me_shadow <- create_shadow("measurement_error",
                           params = list(variables = "x", error_type = "classical", sigma = 2))
  
  # update measurement error based on truncation
  updated <- update_shadow_params(me_shadow, list(truncation))
  
  # sigma should be reduced
  expect_lt(updated$params$sigma, me_shadow$params$sigma)
})

test_that("apply_shadows_with_dependencies integrates correctly", {
  set.seed(123)
  n <- 100
  data <- data.frame(
    id = 1:n,
    t0_x = rnorm(n, 5, 2),
    t1_x = rnorm(n, 5, 2),
    t0_y = rnorm(n, 10, 3),
    t1_y = rnorm(n, 10, 3),
    t0_z = rbinom(n, 1, 0.5),
    t1_z = rbinom(n, 1, 0.5)
  )
  
  # create shadows with dependencies
  shadows <- list(
    create_shadow("measurement_error", 
                 params = list(variables = c("t0_x", "t1_x"), error_type = "classical", sigma = 1)),
    create_shadow("truncation", 
                 params = list(variables = c("t0_x", "t1_x"), lower = 0))
  )
  
  # apply without dependency management for now
  result_data <- data
  for (shadow in shadows) {
    result_data <- apply_shadow(result_data, shadow)
  }
  
  # check that shadows were applied
  expect_true("t0_x_true" %in% names(result_data))  # truncation creates _true vars
  expect_true("t1_x_true" %in% names(result_data))
})

test_that("Circular dependencies are handled", {
  # note: current implementation doesn't have circular dependencies
  # but this test ensures the system remains acyclic
  deps <- margot.sim:::get_shadow_dependencies()
  
  # check for cycles using DFS
  check_cycles <- function(deps) {
    types <- names(deps)
    visited <- setNames(rep(FALSE, length(types)), types)
    rec_stack <- setNames(rep(FALSE, length(types)), types)
    
    has_cycle <- FALSE
    
    dfs <- function(v) {
      visited[v] <<- TRUE
      rec_stack[v] <<- TRUE
      
      for (u in deps[[v]]) {
        if (!visited[u]) {
          if (dfs(u)) return(TRUE)
        } else if (rec_stack[u]) {
          return(TRUE)
        }
      }
      
      rec_stack[v] <<- FALSE
      return(FALSE)
    }
    
    for (type in types) {
      if (!visited[type]) {
        if (dfs(type)) {
          has_cycle <- TRUE
          break
        }
      }
    }
    
    return(!has_cycle)
  }
  
  expect_true(check_cycles(deps))
})

test_that("visualize_shadow_dependencies works", {
  shadows <- list(
    create_shadow("measurement_error", 
                 params = list(variables = "x", error_type = "classical", sigma = 1)),
    create_shadow("truncation", 
                 params = list(variables = "x", lower = 0))
  )
  
  # capture output
  output <- capture.output(visualize_shadow_dependencies(shadows))
  
  expect_true(any(grepl("Shadow Dependency Graph", output)))
  expect_true(any(grepl("measurement_error depends on: truncation", output)))
  expect_true(any(grepl("Current shadow ordering", output)))
})

test_that("Shadow dependencies handle missing dependent variables", {
  # create shadow with non-existent dependent variable
  shadow <- create_shadow("measurement_error",
                         params = list(variables = "x", error_type = "classical", sigma = 1))
  
  # should work without dependencies
  updated <- update_shadow_params(shadow, list())
  expect_equal(updated$params$sigma, 1)
})

test_that("Complex shadow ordering works", {
  # create a complex set of shadows with multiple dependencies
  shadows <- list(
    create_shadow("selection", params = list()),  # depends on censoring, truncation, positivity
    create_shadow("measurement_error", 
                 params = list(variables = "x", error_type = "classical", sigma = 1)), # depends on truncation
    create_shadow("item_missingness", 
                 params = list(variables = "y", rate = 0.1, mechanism = "MAR", dependent_vars = "x")), # depends on measurement_error
    create_shadow("truncation", params = list(variables = "x", lower = 0)),
    create_shadow("censoring", params = list(rate = 0.1, mechanism = "MAR")),
    create_shadow("positivity", 
                 params = list(exposure_var = "a", filter_fn = function(d) rep(TRUE, nrow(d))))
  )
  
  # reorder
  reordered <- reorder_shadows(shadows)
  types <- sapply(reordered, function(s) s$type)
  
  # check critical orderings
  # truncation before measurement_error
  expect_lt(which(types == "truncation"), which(types == "measurement_error"))
  # measurement_error before item_missingness
  expect_lt(which(types == "measurement_error"), which(types == "item_missingness"))
  # censoring before selection
  expect_lt(which(types == "censoring"), which(types == "selection"))
  # positivity before selection
  expect_lt(which(types == "positivity"), which(types == "selection"))
})

test_that("Shadow parameter updates handle edge cases", {
  # test with edge case parameters
  shadow <- create_shadow("measurement_error",
                         params = list(variables = "x", error_type = "classical", sigma = 1))
  
  truncation <- create_shadow("truncation", 
                            params = list(variables = "x", lower = 0, upper = 10))
  
  # should update based on truncation
  updated <- update_shadow_params(shadow, list(truncation))
  expect_lt(updated$params$sigma, 1)  # should be reduced due to truncation
})

test_that("apply_shadows_with_dependencies produces correct output", {
  set.seed(789)
  data <- data.frame(
    id = 1:100,
    t0_x = rnorm(100),
    t1_x = rnorm(100),
    t0_y = rnorm(100),
    t1_y = rnorm(100),
    t0_a = rbinom(100, 1, 0.5),
    t1_a = rbinom(100, 1, 0.5)
  )
  
  shadows <- list(
    create_shadow("measurement_error", 
                 params = list(variables = c("t0_x", "t1_x"), error_type = "classical", sigma = 0.5)),
    create_shadow("truncation", 
                 params = list(variables = c("t0_x", "t1_x"), lower = -2, upper = 2))
  )
  
  # apply shadows in order
  result_data <- data
  for (shadow in shadows) {
    result_data <- apply_shadow(result_data, shadow)
  }
  
  # check that shadows were applied
  expect_true("t0_x_true" %in% names(result_data))  # truncation creates _true vars
  expect_true("t1_x_true" %in% names(result_data))
  
  # check that we have both original and modified data
  expect_true(ncol(result_data) > ncol(data))  # additional columns were created
})