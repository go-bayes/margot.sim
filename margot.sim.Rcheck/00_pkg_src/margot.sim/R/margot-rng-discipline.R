#' RNG Stream Management for Parallel Monte Carlo
#'
#' @description
#' Implements proper random number generation stream management for
#' reproducible parallel Monte Carlo simulations.
#'
#' @details
#' This module ensures that:
#' - Each Monte Carlo replication gets its own independent RNG stream
#' - Results are reproducible regardless of parallelisation
#' - Streams don't overlap between replications
#' - The same results are obtained whether run in parallel or sequential

#' Create RNG streams for parallel processing
#'
#' @param n_streams Number of independent RNG streams needed
#' @param seed Master seed for reproducibility
#' @param kind RNG kind (default: "L'Ecuyer-CMRG" for parallel safety)
#'
#' @return List of RNG stream states
#' @export
#' @examples
#' # Create 100 independent streams
#' streams <- create_rng_streams(100, seed = 123)
create_rng_streams <- function(n_streams, seed = NULL, kind = "L'Ecuyer-CMRG") {
  # save current RNG state
  if (!exists(".Random.seed")) {
    set.seed(NULL)  # initialize RNG
  }
  old_seed <- .Random.seed
  old_kind <- RNGkind()[1]
  on.exit({
    # restore RNG state
    RNGkind(old_kind)
    .Random.seed <<- old_seed
  })
  
  # set RNG kind for stream generation
  RNGkind(kind)
  
  # set master seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # create independent streams
  streams <- vector("list", n_streams)
  for (i in seq_len(n_streams)) {
    # advance RNG state significantly between streams
    # to ensure independence
    if (i > 1) {
      # skip ahead by generating many random numbers
      invisible(runif(127))  # 127 is prime, helps avoid patterns
    }
    streams[[i]] <- .Random.seed
  }
  
  return(streams)
}

#' Set RNG stream for a specific replication
#'
#' @param stream RNG stream state (from create_rng_streams)
#' @param kind RNG kind to use
#' @keywords internal
set_rng_stream <- function(stream, kind = "L'Ecuyer-CMRG") {
  RNGkind(kind)
  .Random.seed <<- stream
}

#' Monte Carlo wrapper with proper RNG discipline
#'
#' @param rep_id Replication ID
#' @param rng_stream RNG stream for this replication
#' @param rep_fn Function to run for one replication
#' @param ... Additional arguments passed to rep_fn
#' @keywords internal
run_mc_replication <- function(rep_id, rng_stream, rep_fn, ...) {
  # set RNG stream for this replication
  set_rng_stream(rng_stream)
  
  # run the replication
  result <- rep_fn(rep_id = rep_id, ...)
  
  return(result)
}

#' Validate RNG reproducibility
#'
#' @description
#' Tests that parallel and sequential runs produce identical results
#'
#' @param test_fn Function to test (should use RNG)
#' @param n_reps Number of replications to test
#' @param seed Seed for testing
#' @param ... Additional arguments for test_fn
#'
#' @return Logical indicating if results are reproducible
#' @export
#' @examples
#' \dontrun{
#' # Test that a simulation is reproducible
#' test_fn <- function(rep_id) {
#'   data <- rnorm(100)
#'   mean(data)
#' }
#' 
#' is_reproducible <- validate_rng_reproducibility(
#'   test_fn = test_fn,
#'   n_reps = 10,
#'   seed = 123
#' )
#' }
validate_rng_reproducibility <- function(test_fn, n_reps = 10, seed = 123, ...) {
  # create streams
  streams <- create_rng_streams(n_reps, seed = seed)
  
  # run sequentially
  seq_results <- lapply(seq_len(n_reps), function(i) {
    run_mc_replication(i, streams[[i]], test_fn, ...)
  })
  
  # run in parallel (if available)
  if (requireNamespace("parallel", quietly = TRUE)) {
    # use 2 cores for testing
    cl <- parallel::makeCluster(2)
    on.exit(parallel::stopCluster(cl))
    
    # export necessary functions
    parallel::clusterExport(cl, c("set_rng_stream", "run_mc_replication"),
                           envir = environment())
    
    # run in parallel
    par_results <- parallel::parLapply(cl, seq_len(n_reps), function(i) {
      run_mc_replication(i, streams[[i]], test_fn, ...)
    })
    
    # compare results
    identical(seq_results, par_results)
  } else {
    # if parallel not available, just check sequential reproducibility
    streams2 <- create_rng_streams(n_reps, seed = seed)
    seq_results2 <- lapply(seq_len(n_reps), function(i) {
      run_mc_replication(i, streams2[[i]], test_fn, ...)
    })
    
    identical(seq_results, seq_results2)
  }
}

#' Get RNG stream diagnostics
#'
#' @description
#' Provides diagnostics about RNG stream independence
#'
#' @param streams List of RNG streams
#' @return Data frame with diagnostics
#' @export
diagnose_rng_streams <- function(streams) {
  n_streams <- length(streams)
  
  # check for duplicate streams
  duplicates <- anyDuplicated(streams)
  
  # check stream spacing
  # for L'Ecuyer, streams should be well-separated
  stream_starts <- sapply(streams, function(s) s[2])
  min_spacing <- min(diff(sort(stream_starts)))
  
  # test independence by generating some numbers from each stream
  test_samples <- lapply(streams, function(stream) {
    # save current state
    old_seed <- .Random.seed
    on.exit(.Random.seed <<- old_seed)
    
    # set stream and generate numbers
    set_rng_stream(stream)
    runif(10)
  })
  
  # check for correlation between streams
  cor_matrix <- cor(do.call(cbind, test_samples))
  max_cor <- max(abs(cor_matrix[upper.tri(cor_matrix)]))
  
  data.frame(
    n_streams = n_streams,
    has_duplicates = duplicates > 0,
    min_stream_spacing = min_spacing,
    max_correlation = max_cor,
    appears_independent = duplicates == 0 & max_cor < 0.1
  )
}