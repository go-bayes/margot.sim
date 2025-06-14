#' Memory Management Utilities for Monte Carlo Simulations
#'
#' @description
#' Functions to help manage memory usage during large Monte Carlo simulations,
#' including data summarization, streaming results, and memory monitoring.

#' Create a summary function for data reduction
#'
#' @param stats Character vector of statistics to compute
#' @param vars Character vector of variables to summarize (NULL for all)
#' @return Function that summarizes data
#' @export
#' @examples
#' # Create a summarizer that computes means and SDs
#' summarizer <- create_data_summarizer(
#'   stats = c("mean", "sd", "quantiles"),
#'   vars = c("y", "a")
#' )
#' 
#' # Use in Monte Carlo
#' \dontrun{
#' results <- margot_monte_carlo(
#'   n_reps = 1000,
#'   n_per_rep = 100,
#'   dgp_params = list(
#'     waves = 2,
#'     treatments = "a",
#'     interventions = list(
#'       natural = function(data, time, trt) data[[trt]],
#'       shifted = function(data, time, trt) pmin(data[[trt]] + 1, 2)
#'     )
#'   ),
#'   estimator_fn = function(data) list(estimate = mean(data$t2_y)),
#'   summarize_fn = summarizer
#' )
#' }
create_data_summarizer <- function(stats = c("mean", "sd", "quantiles"),
                                 vars = NULL) {
  stats <- match.arg(stats, several.ok = TRUE,
                    choices = c("mean", "sd", "quantiles", "cor", "counts"))
  
  function(complete, shadowed) {
    # select variables to summarize
    if (!is.null(vars)) {
      complete_vars <- intersect(vars, names(complete))
      shadowed_vars <- intersect(vars, names(shadowed))
    } else {
      complete_vars <- names(complete)
      shadowed_vars <- names(shadowed)
    }
    
    summary_list <- list()
    
    # summarize complete data
    if ("mean" %in% stats) {
      summary_list$complete_means <- sapply(complete[complete_vars], 
                                          function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)
      summary_list$shadowed_means <- sapply(shadowed[shadowed_vars], 
                                          function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)
    }
    
    if ("sd" %in% stats) {
      summary_list$complete_sds <- sapply(complete[complete_vars], 
                                        function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA)
      summary_list$shadowed_sds <- sapply(shadowed[shadowed_vars], 
                                        function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA)
    }
    
    if ("quantiles" %in% stats) {
      summary_list$complete_quantiles <- lapply(complete[complete_vars], function(x) {
        if(is.numeric(x)) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE) else NA
      })
      summary_list$shadowed_quantiles <- lapply(shadowed[shadowed_vars], function(x) {
        if(is.numeric(x)) quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE) else NA
      })
    }
    
    if ("cor" %in% stats) {
      numeric_complete <- complete[sapply(complete, is.numeric)]
      numeric_shadowed <- shadowed[sapply(shadowed, is.numeric)]
      
      if (ncol(numeric_complete) > 1) {
        summary_list$complete_cor <- cor(numeric_complete, use = "complete.obs")
      }
      if (ncol(numeric_shadowed) > 1) {
        summary_list$shadowed_cor <- cor(numeric_shadowed, use = "complete.obs")
      }
    }
    
    if ("counts" %in% stats) {
      summary_list$n_complete <- nrow(complete)
      summary_list$n_shadowed <- nrow(shadowed)
      summary_list$n_missing <- sum(is.na(shadowed))
    }
    
    summary_list
  }
}

#' Monitor memory usage during simulation
#'
#' @param interval Check interval in seconds
#' @param threshold Memory threshold in MB to trigger warning
#' @return Function that monitors memory
#' @export
create_memory_monitor <- function(interval = 10, threshold = 1000) {
  last_check <- Sys.time()
  
  function() {
    current_time <- Sys.time()
    if (difftime(current_time, last_check, units = "secs") > interval) {
      mem_used <- as.numeric(gc()[2, 2])
      if (mem_used > threshold) {
        warning(sprintf("High memory usage: %.1f MB (threshold: %.1f MB)", 
                       mem_used, threshold))
      }
      last_check <<- current_time
    }
  }
}

#' Stream Monte Carlo results to disk
#'
#' @param file_path Path to output file
#' @param batch_size Number of results to accumulate before writing
#' @return List with write and finalize functions
#' @export
create_mc_streamer <- function(file_path, batch_size = 100) {
  # initialize
  batch <- list()
  batch_count <- 0
  total_written <- 0
  
  # ensure directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  # write function
  write_fn <- function(result) {
    batch[[length(batch) + 1]] <<- result
    
    if (length(batch) >= batch_size) {
      # convert to data frame and write
      batch_df <- do.call(rbind, lapply(batch, function(x) {
        x$data <- NULL  # remove large data elements
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
      
      if (total_written == 0) {
        # first write - create file
        write.csv(batch_df, file_path, row.names = FALSE)
      } else {
        # append to existing file
        write.table(batch_df, file_path, append = TRUE, 
                   sep = ",", row.names = FALSE, col.names = FALSE)
      }
      
      total_written <<- total_written + nrow(batch_df)
      batch <<- list()
      gc()  # force garbage collection
    }
  }
  
  # finalize function
  finalize_fn <- function() {
    if (length(batch) > 0) {
      # write remaining results
      batch_df <- do.call(rbind, lapply(batch, function(x) {
        x$data <- NULL
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
      
      if (total_written == 0) {
        write.csv(batch_df, file_path, row.names = FALSE)
      } else {
        write.table(batch_df, file_path, append = TRUE,
                   sep = ",", row.names = FALSE, col.names = FALSE)
      }
      
      total_written <<- total_written + nrow(batch_df)
    }
    
    message(sprintf("Streamed %d results to %s", total_written, file_path))
    return(total_written)
  }
  
  list(
    write = write_fn,
    finalize = finalize_fn,
    get_count = function() total_written
  )
}

#' Resume Monte Carlo simulation from checkpoint
#'
#' @param checkpoint_dir Directory containing checkpoint files
#' @param n_reps Total number of replications desired
#' @param ... Additional arguments passed to margot_monte_carlo
#' @return Monte Carlo results
#' @export
resume_monte_carlo <- function(checkpoint_dir, n_reps, ...) {
  if (!dir.exists(checkpoint_dir)) {
    stop("Checkpoint directory does not exist")
  }
  
  # find checkpoint files
  checkpoint_files <- list.files(checkpoint_dir, 
                               pattern = "checkpoint_.*\\.rds",
                               full.names = TRUE)
  
  if (length(checkpoint_files) == 0) {
    message("No checkpoints found, starting from beginning")
    return(margot_monte_carlo(n_reps = n_reps, 
                            checkpoint_dir = checkpoint_dir, ...))
  }
  
  # find highest checkpoint
  checkpoint_nums <- as.numeric(gsub(".*checkpoint_([0-9]+)\\.rds", "\\1", 
                                   checkpoint_files))
  last_checkpoint <- max(checkpoint_nums)
  
  message(sprintf("Resuming from checkpoint %d", last_checkpoint))
  
  # load existing results
  existing_results <- lapply(1:last_checkpoint, function(i) {
    file <- sprintf("%s/checkpoint_%06d.rds", checkpoint_dir, i)
    if (file.exists(file)) {
      readRDS(file)
    } else {
      NULL
    }
  })
  existing_results <- existing_results[!sapply(existing_results, is.null)]
  
  # run remaining replications
  remaining <- n_reps - last_checkpoint
  if (remaining > 0) {
    new_results <- margot_monte_carlo(
      n_reps = remaining,
      checkpoint_dir = checkpoint_dir,
      ...
    )
    
    # combine results
    all_results <- c(existing_results, new_results$results)
  } else {
    all_results <- existing_results
  }
  
  # return combined results
  structure(
    list(
      results = all_results,
      params = list(n_reps = n_reps, checkpoint_dir = checkpoint_dir)
    ),
    class = "margot_mc_results"
  )
}

#' Get memory usage statistics
#'
#' @return Data frame with memory statistics
#' @export
get_memory_stats <- function() {
  gc_info <- gc()
  
  data.frame(
    used_mb = gc_info[2, 2],
    gc_trigger_mb = gc_info[2, 3],
    max_used_mb = gc_info[2, 4],
    usage_ratio = gc_info[2, 2] / gc_info[2, 3]
  )
}