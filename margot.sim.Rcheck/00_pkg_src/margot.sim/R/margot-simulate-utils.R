#' Report margot simulation summary
#'
#' @description
#' Provides a formatted summary of a margot_simulate() output object,
#' including structural model details, sample sizes, and censoring patterns.
#'
#' @param dat A data frame or tibble output from \code{margot_simulate}
#'
#' @return Invisibly returns the metadata list. Called for its side effect
#'   of printing a formatted summary to the console.
#'
#' @examples
#' dat <- margot_simulate(n = 100, waves = 3, seed = 123)
#' margot_report_sim(dat)
#'
#' @export
margot_report_sim <- function(dat) {
  meta <- attr(dat, "margot_meta")

  if (is.null(meta)) {
    stop("data does not appear to be from margot_simulate (no margot_meta attribute)")
  }

  cli::cli_h1("margot simulation summary")
  cli::cli_h2("design")
  cli::cli_bullets(c(
    "*" = "n = {meta$n} subjects",
    "*" = "waves = {meta$waves} measurement occasions",
    "*" = "structural model: {meta$structural_model}",
    "*" = "intervention: {ifelse(meta$intervention_applied, 'yes', 'no')}",
    "*" = "sampling weights: {ifelse(meta$sampling_weights_applied, 'yes', 'no')}"
  ))

  cli::cli_h2("variables")
  cli::cli_bullets(c(
    "*" = "baseline covariates: {meta$n_baselines}",
    "*" = "exposure type: {meta$exposure_type}",
    "*" = "outcome type: {meta$outcome_type} ({meta$n_outcomes} outcome{?s})",
    "*" = "outcome feedback: {meta$y_feedback}"
  ))

  if (length(meta$censoring_probs) > 0) {
    cli::cli_h2("censoring")
    cli::cli_bullets(c(
      "*" = "censoring mechanism: built-in",
      "*" = "base rate: {meta$params$cens_rate}"
    ))

    if (!is.null(meta$censoring_summary)) {
      cli::cli_alert_info("actual censoring patterns:")
      for (wave in names(meta$censoring_summary)) {
        tab <- meta$censoring_summary[[wave]]
        prop_retained <- if ("1" %in% names(tab)) tab["1"] / sum(tab) else 0
        cli::cli_bullets(c(
          " " = "{wave}: {round(prop_retained * 100, 1)}% retained"
        ))
      }
    }
  } else if (!is.null(meta$censoring_probs) && length(meta$censoring_probs) > 0) {
    cli::cli_alert_warning("censoring: probabilities stored but not yet applied")
    cli::cli_alert_info("use apply_censoring_post_hoc() to apply censoring")
  }

  if (meta$format == "long") {
    cli::cli_alert_info("data format: long (id-time)")
  } else {
    cli::cli_alert_info("data format: wide (id only)")
  }

  invisible(meta)
}

#' Convert margot data between wide and long formats
#'
#' @param data Data from margot_simulate
#' @param format Target format: "wide" or "long"
#'
#' @return Data in requested format with metadata preserved
#' @export
margot_convert_format <- function(data, format = c("wide", "long")) {
  format <- match.arg(format)
  meta <- attr(data, "margot_meta")
  
  if (is.null(meta)) {
    stop("data must have margot_meta attribute")
  }
  
  current_format <- meta$format
  
  if (current_format == format) {
    message("data is already in ", format, " format")
    return(data)
  }
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package is required for format conversion")
  }
  
  if (format == "long") {
    # wide to long
    baseline_vars <- c("id", grep("^b[0-9]+$", names(data), value = TRUE))
    if ("sampling_weight" %in% names(data)) {
      baseline_vars <- c(baseline_vars, "sampling_weight")
    }
    
    long_data <- tidyr::pivot_longer(
      data,
      cols = -tidyr::all_of(baseline_vars),
      names_to = c("time", ".value"),
      names_pattern = "t([0-9]+)_(.*)"
    ) |>
      dplyr::mutate(time = as.integer(time)) |>
      dplyr::arrange(id, time)
    
    attr(long_data, "margot_meta") <- meta
    attr(long_data, "margot_meta")$format <- "long"
    
    return(long_data)
    
  } else {
    # long to wide
    id_vars <- c("id", grep("^b[0-9]+$", names(data), value = TRUE))
    if ("sampling_weight" %in% names(data)) {
      id_vars <- c(id_vars, "sampling_weight")
    }
    
    # get all non-id, non-time variables
    value_vars <- setdiff(names(data), c(id_vars, "time"))
    
    wide_data <- tidyr::pivot_wider(
      data,
      id_cols = tidyr::all_of(id_vars),
      names_from = time,
      names_glue = "t{time}_{.value}",
      values_from = tidyr::all_of(value_vars)
    )
    
    attr(wide_data, "margot_meta") <- meta
    attr(wide_data, "margot_meta")$format <- "wide"
    
    return(wide_data)
  }
}

#' Extract variable at specific time point
#'
#' @param data Wide format data from margot_simulate
#' @param var Variable name (without time prefix)
#' @param time Time point
#'
#' @return Vector of values
#' @export
margot_extract_var <- function(data, var, time) {
  col_name <- paste0("t", time, "_", var)
  
  if (!col_name %in% names(data)) {
    stop("variable ", col_name, " not found in data")
  }
  
  data[[col_name]]
}

#' Get summary of missingness patterns
#'
#' @param data Data from margot_simulate
#' @param vars Variables to check (defaults to all)
#'
#' @return Data frame with missingness summary
#' @export
margot_missingness_summary <- function(data, vars = NULL) {
  if (is.null(vars)) {
    # exclude id and baseline covariates
    vars <- grep("^t[0-9]+_", names(data), value = TRUE)
  }
  
  miss_summary <- data.frame(
    variable = vars,
    n_missing = sapply(vars, function(v) sum(is.na(data[[v]]))),
    prop_missing = sapply(vars, function(v) mean(is.na(data[[v]]))),
    stringsAsFactors = FALSE
  )
  
  # add time information
  miss_summary$time <- as.integer(gsub("^t([0-9]+)_.*", "\\1", miss_summary$variable))
  miss_summary$var_name <- gsub("^t[0-9]+_", "", miss_summary$variable)
  
  miss_summary[order(miss_summary$time, miss_summary$var_name), ]
}