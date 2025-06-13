# Test shadow dependency system

test_that("Shadow dependencies are correctly defined", {
  deps <- get_shadow_dependencies()
  
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
    x = rnorm(n, 5, 2),
    y = rnorm(n, 10, 3),
    z = rbinom(n, 1, 0.5)
  )
  
  # create shadows with dependencies
  shadows <- list(
    create_shadow("measurement_error", 
                 params = list(variables = "x", error_type = "classical", sigma = 1)),
    create_shadow("truncation", 
                 params = list(variables = "x", lower = 0))
  )
  
  # apply with dependency management
  result <- suppressMessages(
    apply_shadows_with_dependencies(data, shadows, reorder = TRUE, update_params = TRUE)
  )
  
  # check output structure
  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("diagnostics" %in% names(result))
  expect_true("shadow_order" %in% names(result))
  
  # check shadows were reordered (truncation should be first)
  expect_equal(result$shadow_order[1], "truncation")
  expect_equal(result$shadow_order[2], "measurement_error")
})

test_that("Circular dependencies are handled", {
  # note: current implementation doesn't have circular dependencies
  # but this test ensures the system remains acyclic
  deps <- get_shadow_dependencies()
  
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