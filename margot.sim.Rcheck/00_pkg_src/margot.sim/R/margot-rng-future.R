#' Future-based RNG Management for Monte Carlo
#'
#' @description
#' Implements RNG management using the future package framework for
#' better cross-platform reproducibility and cleaner parallel code.
#'
#' @details
#' This approach leverages the future package's built-in RNG management
#' which automatically handles L'Ecuyer-CMRG streams for parallel execution.

#' Run Monte Carlo simulation with future-based parallelization
#'
#' @param n_reps Number of Monte Carlo replications
#' @param n_per_rep Sample size per replication
#' @param dgp_params List of parameters for data generation
#' @param shadows List of shadow objects to apply
#' @param estimator_fn Function that takes data and returns estimate(s)
#' @param seed Random seed for reproducibility
#' @param plan Future plan (e.g., "multisession", "sequential")
#' @param workers Number of parallel workers
#' @param ... Additional arguments passed to run_one_rep
#'
#' @return List of Monte Carlo results
#' @keywords internal
margot_mc_future <- function(
    n_reps,
    n_per_rep,
    dgp_params,
    shadows = list(),
    estimator_fn,
    seed = NULL,
    plan = "multisession",
    workers = NULL,
    ...
) {
  # Check if future is available
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required for this function. Please install it.")
  }
  
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("Package 'future.apply' is required for this function. Please install it.")
  }
  
  # Set up the future plan
  if (is.null(workers)) {
    workers <- future::availableCores() - 1
  }
  
  # Store old plan to restore on exit
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  # Set new plan
  if (plan == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else if (plan == "sequential") {
    future::plan(future::sequential)
  } else {
    future::plan(plan)
  }
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
    # future.apply will automatically handle RNG streams
  }
  
  # Define function for one replication
  run_one_rep <- function(rep_id) {
    # Generate data
    sim_args <- modifyList(
      list(n = n_per_rep),
      dgp_params
    )
    
    data <- do.call(margot_simulate, sim_args)
    
    # Apply shadows if provided
    if (length(shadows) > 0) {
      data_shadowed <- apply_shadows(data, shadows)
    } else {
      data_shadowed <- data
    }
    
    # Run estimator
    est_result <- estimator_fn(data_shadowed)
    
    # Return results
    c(
      list(rep_id = rep_id),
      est_result
    )
  }
  
  # Run replications using future_lapply
  # This automatically handles RNG streams correctly
  results <- future.apply::future_lapply(
    1:n_reps,
    run_one_rep,
    future.seed = TRUE  # ensures reproducible RNG
  )
  
  return(results)
}

#' Create a seed sequence for reproducible parallel execution
#'
#' @description
#' Creates a sequence of seeds that can be used for reproducible
#' parallel execution without the future package.
#'
#' @param n Number of seeds needed
#' @param master_seed Master seed for generating the sequence
#'
#' @return Integer vector of seeds
#' @export
#' @examples
#' seeds <- create_seed_sequence(10, master_seed = 123)
create_seed_sequence <- function(n, master_seed = NULL) {
  if (!is.null(master_seed)) {
    set.seed(master_seed)
  }
  
  # Generate seeds that are well-separated
  # Use large prime multiplier to avoid correlation
  base <- sample.int(.Machine$integer.max, 1)
  seeds <- ((seq_len(n) - 1) * 1009 + base) %% .Machine$integer.max
  
  return(seeds)
}

#' Set up reproducible RNG for parallel backend
#'
#' @description
#' Configures the RNG for reproducible parallel execution based on
#' the backend being used.
#'
#' @param backend Character string: "future", "parallel", "doParallel"
#' @param seed Master seed
#' @param n_streams Number of RNG streams needed
#'
#' @return Configuration list with RNG setup
#' @export
setup_parallel_rng <- function(backend = "future", seed = NULL, n_streams = NULL) {
  config <- list(
    backend = backend,
    seed = seed
  )
  
  if (backend == "future") {
    # future handles this automatically with future.seed = TRUE
    config$setup <- function() {
      if (!is.null(seed)) set.seed(seed)
    }
    config$note <- "Use future.seed = TRUE in future_lapply"
    
  } else if (backend == "parallel") {
    # Use L'Ecuyer-CMRG for mclapply
    config$setup <- function() {
      RNGkind("L'Ecuyer-CMRG")
      if (!is.null(seed)) set.seed(seed)
    }
    config$note <- "Use mc.set.seed = TRUE in mclapply"
    
  } else if (backend == "doParallel") {
    # Create explicit RNG streams
    if (is.null(n_streams)) {
      stop("n_streams must be specified for doParallel backend")
    }
    
    config$streams <- create_rng_streams(n_streams, seed = seed)
    config$setup <- function(cl) {
      # Export streams to cluster
      parallel::clusterSetRNGStream(cl, seed)
    }
    config$note <- "Use clusterSetRNGStream for proper RNG"
  }
  
  return(config)
}

#' Validate parallel RNG reproducibility
#'
#' @description
#' Tests that a function produces identical results when run with
#' different parallel backends.
#'
#' @param test_fn Function to test
#' @param n_reps Number of replications
#' @param seed Seed for testing
#' @param backends Character vector of backends to test
#'
#' @return Data frame with reproducibility test results
#' @export
#' @examples
#' \dontrun{
#' test_fn <- function(i) {
#'   sum(rnorm(100))
#' }
#' 
#' results <- validate_parallel_rng(
#'   test_fn = test_fn,
#'   n_reps = 10,
#'   seed = 123,
#'   backends = c("sequential", "future")
#' )
#' }
validate_parallel_rng <- function(
    test_fn,
    n_reps = 10,
    seed = 123,
    backends = c("sequential", "future", "parallel")
) {
  results <- list()
  
  # Test sequential execution
  if ("sequential" %in% backends) {
    set.seed(seed)
    results$sequential <- lapply(1:n_reps, test_fn)
  }
  
  # Test future
  if ("future" %in% backends && requireNamespace("future.apply", quietly = TRUE)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan))
    
    future::plan(future::multisession, workers = 2)
    set.seed(seed)
    results$future <- future.apply::future_lapply(
      1:n_reps, 
      test_fn,
      future.seed = TRUE
    )
  }
  
  # Test parallel::mclapply
  if ("parallel" %in% backends && .Platform$OS.type != "windows") {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
    results$parallel <- parallel::mclapply(
      1:n_reps,
      test_fn,
      mc.cores = 2,
      mc.set.seed = TRUE
    )
  }
  
  # Compare results
  if (length(results) < 2) {
    return(data.frame(
      comparison = "insufficient_backends",
      identical = NA,
      max_diff = NA
    ))
  }
  
  # Check if all results are identical
  comparisons <- list()
  backend_names <- names(results)
  
  for (i in 1:(length(backend_names) - 1)) {
    for (j in (i + 1):length(backend_names)) {
      backend1 <- backend_names[i]
      backend2 <- backend_names[j]
      
      # Extract numeric results for comparison
      vals1 <- unlist(results[[backend1]])
      vals2 <- unlist(results[[backend2]])
      
      comparisons[[paste(backend1, "vs", backend2)]] <- data.frame(
        comparison = paste(backend1, "vs", backend2),
        identical = identical(vals1, vals2),
        max_diff = max(abs(vals1 - vals2))
      )
    }
  }
  
  do.call(rbind, comparisons)
}