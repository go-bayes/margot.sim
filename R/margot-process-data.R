#' Process longitudinal data in wide format with censoring
#'
#' @description
#' Internal replacement for margot_process_longitudinal_data_wider().
#' Processes longitudinal data in wide format across multiple waves,
#' handling censoring indicators and optional scaling.
#'
#' @details
#' ## Censoring Propagation
#' 
#' This function enforces monotonic missingness patterns:
#' - Once a subject is lost (has no data at wave k), they remain lost
#' - All future waves are set to NA for consistency
#' - This prevents "resurrections" where subjects reappear after being lost
#' 
#' ## Not-Lost Indicators
#' 
#' For each wave k, the indicator `tk_not_lost_following_wave` is created:
#' - Value = 1: Subject has at least some data at wave k+1
#' - Value = 0: Subject has no data at wave k+1 (censored)
#' 
#' These indicators are essential for:
#' - Constructing inverse probability weights
#' - Defining the at-risk set for each wave
#' - Implementing proper survival analysis methods
#' 
#' ## No Carry-Forward
#' 
#' This function does NOT implement last observation carried forward (LOCF).
#' Missing values due to censoring remain as NA. This is intentional because:
#' - LOCF can introduce bias in longitudinal causal inference
#' - Modern methods (IPCW, g-computation) handle missingness more appropriately
#' - Carrying forward values can mask the true censoring pattern
#'
#' @param df_wide Wide-format data frame containing time-prefixed columns (e.g., `t0_x`)
#' @param exposure_vars Character vector of exposure base names (without time prefixes)
#' @param outcome_vars Character vector of outcome base names (without time prefixes)
#' @param preserve_temporal_order Logical; if TRUE, maintains temporal ordering (t0_var, t1_var, ..., tn_var)
#' @param scale_continuous Logical; if TRUE, scales continuous variables. Default: FALSE
#' @param not_lost_suffix Suffix for the not-lost indicator. Default: "not_lost_following_wave"
#' @param time_point_regex Regex pattern to identify time-point prefixes. Default: "^(t\\d+)_.*$"
#'
#' @return A data frame with processed columns and censoring indicators. Future waves
#'   are set to NA for censored subjects (no carry-forward).
#'   
#' @keywords internal
margot_process_longitudinal <- function(
    df_wide,
    exposure_vars = NULL,
    outcome_vars = NULL,
    preserve_temporal_order = TRUE,
    scale_continuous = FALSE,
    not_lost_suffix = "not_lost_following_wave",
    time_point_regex = "^(t\\d+)_.*$"
) {
  # input validation
  if (!is.data.frame(df_wide)) {
    stop("df_wide must be a data frame")
  }
  
  df <- df_wide
  
  # discover time points
  matched_cols <- grep(time_point_regex, colnames(df), value = TRUE)
  time_points <- unique(gsub(time_point_regex, "\\1", matched_cols))
  time_points <- time_points[order(as.numeric(sub("^t", "", time_points)))]
  
  num_time_points <- length(time_points)
  
  if (num_time_points < 2) {
    stop("at least 2 time points required for longitudinal data")
  }
  
  # create not-lost indicators based on missingness patterns
  for (i in seq_len(num_time_points - 1)) {
    t0 <- time_points[i]
    t1 <- time_points[i + 1]
    
    # create not_lost indicator column name
    nl_col <- paste0(t0, "_", not_lost_suffix)
    
    # get columns for next time point
    t1_cols <- grep(paste0("^", t1, "_"), names(df), value = TRUE)
    
    if (length(t1_cols) > 0) {
      # check if any data exists at next time point
      # not lost = has at least some data at next time point
      not_lost <- rowSums(!is.na(df[, t1_cols, drop = FALSE])) > 0
      df[[nl_col]] <- as.integer(not_lost)
    } else {
      # no columns for next time point means everyone is censored
      df[[nl_col]] <- 0L
    }
  }
  
  # apply censoring: if lost at time t, set all future data to NA
  for (i in seq_len(num_time_points - 1)) {
    t0 <- time_points[i]
    nl_col <- paste0(t0, "_", not_lost_suffix)
    
    # check who is lost
    lost <- df[[nl_col]] == 0 | is.na(df[[nl_col]])
    
    if (any(lost)) {
      # censor all future waves
      for (j in (i + 1):num_time_points) {
        future_t <- time_points[j]
        future_cols <- grep(paste0("^", future_t, "_"), names(df), value = TRUE)
        
        # exclude not_lost indicators from censoring
        future_cols <- setdiff(future_cols, grep(not_lost_suffix, future_cols, value = TRUE))
        
        if (length(future_cols) > 0) {
          df[lost, future_cols] <- NA
        }
      }
    }
  }
  
  # optional: scale continuous variables
  if (scale_continuous) {
    # identify numeric columns
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    
    # exclude special columns from scaling
    exclude_pattern <- paste0("_", not_lost_suffix, "$|_binary$|^id$")
    to_scale <- num_cols[!grepl(exclude_pattern, num_cols)]
    
    # exclude exposure variables if specified
    if (!is.null(exposure_vars)) {
      expo_names <- unlist(lapply(time_points, function(t) paste0(t, "_", exposure_vars)))
      to_scale <- setdiff(to_scale, expo_names)
    }
    
    # scale the columns
    for (col in to_scale) {
      scaled_vals <- scale(df[[col]])
      # preserve NA values
      df[[paste0(col, "_z")]] <- as.vector(scaled_vals)
    }
    
    # remove original unscaled columns
    df <- df[, !(names(df) %in% to_scale)]
  }
  
  # reorder columns if preserve_temporal_order is TRUE
  if (preserve_temporal_order) {
    # build ordered column list
    ordered_cols <- character()
    
    # id column first
    if ("id" %in% names(df)) {
      ordered_cols <- c(ordered_cols, "id")
    }
    
    # baseline covariates (those without time prefix)
    base_cols <- names(df)[!grepl(time_point_regex, names(df))]
    base_cols <- setdiff(base_cols, c("id", ordered_cols))
    ordered_cols <- c(ordered_cols, base_cols)
    
    # time-varying columns in temporal order
    for (t in time_points) {
      t_cols <- grep(paste0("^", t, "_"), names(df), value = TRUE)
      
      # separate special columns
      nl_cols <- grep(not_lost_suffix, t_cols, value = TRUE)
      other_cols <- setdiff(t_cols, nl_cols)
      
      # add in order: regular columns, then not_lost indicators
      ordered_cols <- c(ordered_cols, other_cols, nl_cols)
    }
    
    # reorder dataframe
    ordered_cols <- intersect(ordered_cols, names(df))
    df <- df[, ordered_cols]
  }
  
  # add metadata as attributes
  attr(df, "time_points") <- time_points
  attr(df, "n_waves") <- num_time_points
  attr(df, "processed") <- TRUE
  
  df
}