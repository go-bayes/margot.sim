#' Shadow Dependency System
#'
#' @description
#' Implements a system for tracking and managing dependencies between shadows.
#' Some shadows affect the parameters or behavior of other shadows.
#'
#' @details
#' Examples of shadow dependencies:
#' - Truncation affects the variance available for measurement error
#' - Censoring affects which units are available for selection
#' - Mode effects may interact with measurement error
#' - Positivity violations affect all downstream shadows

#' Define shadow dependency relationships
#'
#' @return List defining which shadows depend on which others
#' @keywords internal
get_shadow_dependencies <- function() {
  list(
    # measurement error depends on truncation (reduced variance)
    measurement_error = c("truncation", "coarsening"),
    
    # selection depends on what data is available
    selection = c("censoring", "truncation", "positivity"),
    
    # item missingness can depend on measurement quality
    item_missingness = c("measurement_error", "mode_effects"),
    
    # mode effects can interact with measurement
    mode_effects = c("measurement_error"),
    
    # censoring affects everything downstream
    censoring = character(0),  # no dependencies
    
    # truncation is usually applied early
    truncation = character(0),
    
    # coarsening affects measurement precision
    coarsening = character(0),
    
    # positivity is fundamental
    positivity = character(0)
  )
}

#' Check shadow ordering for dependencies
#'
#' @param shadows List of shadow objects
#' @return Logical indicating if ordering respects dependencies
#' @export
check_shadow_ordering <- function(shadows) {
  if (length(shadows) <= 1) return(TRUE)
  
  # get shadow types
  shadow_types <- sapply(shadows, function(s) s$type)
  dependencies <- get_shadow_dependencies()
  
  # check each shadow's dependencies appear before it
  for (i in seq_along(shadows)) {
    current_type <- shadow_types[i]
    deps <- dependencies[[current_type]]
    
    if (length(deps) > 0) {
      # find positions of dependencies
      dep_positions <- which(shadow_types %in% deps)
      
      # check all dependencies appear before current shadow
      if (any(dep_positions > i)) {
        warning(sprintf(
          "Shadow '%s' at position %d depends on shadows that appear later: %s",
          current_type, i, 
          paste(shadow_types[dep_positions[dep_positions > i]], collapse = ", ")
        ))
        return(FALSE)
      }
    }
  }
  
  TRUE
}

#' Reorder shadows to respect dependencies
#'
#' @param shadows List of shadow objects
#' @return Reordered list of shadows
#' @export
reorder_shadows <- function(shadows) {
  if (length(shadows) <= 1) return(shadows)
  
  shadow_types <- sapply(shadows, function(s) s$type)
  dependencies <- get_shadow_dependencies()
  
  # create dependency graph
  n <- length(shadows)
  dep_matrix <- matrix(FALSE, n, n)
  
  for (i in seq_len(n)) {
    deps <- dependencies[[shadow_types[i]]]
    if (length(deps) > 0) {
      # mark dependencies
      for (j in seq_len(n)) {
        if (shadow_types[j] %in% deps) {
          dep_matrix[i, j] <- TRUE  # i depends on j
        }
      }
    }
  }
  
  # topological sort
  visited <- rep(FALSE, n)
  order <- integer(0)
  
  # depth-first search
  dfs <- function(v) {
    visited[v] <<- TRUE
    # visit all dependencies first
    for (u in which(dep_matrix[v, ])) {
      if (!visited[u]) {
        dfs(u)
      }
    }
    order <<- c(order, v)
  }
  
  # visit all nodes
  for (i in seq_len(n)) {
    if (!visited[i]) {
      dfs(i)
    }
  }
  
  # return shadows in dependency order
  shadows[order]
}

#' Update shadow parameters based on dependencies
#'
#' @param shadow Shadow object to update
#' @param applied_shadows List of already applied shadows
#' @return Updated shadow object
#' @export
update_shadow_params <- function(shadow, applied_shadows = list()) {
  if (length(applied_shadows) == 0) return(shadow)
  
  # get types of already applied shadows
  applied_types <- sapply(applied_shadows, function(s) s$type)
  
  # update based on shadow type
  if (shadow$type == "measurement_error" && "truncation" %in% applied_types) {
    # truncation reduces variance available for measurement error
    truncation_shadow <- applied_shadows[[which(applied_types == "truncation")[1]]]
    
    if (!is.null(truncation_shadow$params$lower) || !is.null(truncation_shadow$params$upper)) {
      # reduce measurement error variance if data is truncated
      if (shadow$params$error_type == "classical" && !is.null(shadow$params$sigma)) {
        # reduce sigma by truncation strength
        truncation_factor <- 0.8  # could be estimated from truncation bounds
        shadow$params$sigma <- shadow$params$sigma * truncation_factor
        
        message(sprintf(
          "Reduced measurement error sigma from %.3f to %.3f due to truncation",
          shadow$params$sigma / truncation_factor,
          shadow$params$sigma
        ))
      }
    }
  }
  
  if (shadow$type == "selection" && "censoring" %in% applied_types) {
    # censoring affects which units are available for selection
    censoring_shadow <- applied_shadows[[which(applied_types == "censoring")[1]]]
    
    if (!is.null(censoring_shadow$params$rate)) {
      # adjust selection probability based on censoring
      if (!is.null(shadow$params$prob)) {
        # reduce selection probability for censored units
        shadow$params$censoring_adjustment <- 1 - censoring_shadow$params$rate
        
        message(sprintf(
          "Adjusted selection probability for %.1f%% censoring",
          censoring_shadow$params$rate * 100
        ))
      }
    }
  }
  
  shadow
}

#' Apply shadows with dependency management
#'
#' @param data Data to apply shadows to
#' @param shadows List of shadow objects
#' @param reorder Logical, whether to reorder shadows by dependencies
#' @param update_params Logical, whether to update parameters based on dependencies
#' @return List with shadowed data and diagnostics
#' @export
apply_shadows_with_dependencies <- function(data, shadows, 
                                          reorder = TRUE, 
                                          update_params = TRUE) {
  if (length(shadows) == 0) {
    return(list(data = data, diagnostics = list()))
  }
  
  # reorder if requested
  if (reorder) {
    shadows <- reorder_shadows(shadows)
    message("Reordered shadows to respect dependencies")
  }
  
  # check ordering
  if (!check_shadow_ordering(shadows)) {
    warning("Shadow ordering may not respect all dependencies")
  }
  
  # apply shadows sequentially
  current_data <- data
  applied_shadows <- list()
  diagnostics <- list()
  
  for (i in seq_along(shadows)) {
    shadow <- shadows[[i]]
    
    # update parameters based on previously applied shadows
    if (update_params && length(applied_shadows) > 0) {
      shadow <- update_shadow_params(shadow, applied_shadows)
    }
    
    # apply shadow
    result <- apply_shadow(current_data, shadow)
    current_data <- result$data
    
    # store for dependency tracking
    applied_shadows[[length(applied_shadows) + 1]] <- shadow
    
    # collect diagnostics
    diagnostics[[shadow$name %||% paste0("shadow_", i)]] <- list(
      type = shadow$type,
      params = shadow$params,
      dependencies = get_shadow_dependencies()[[shadow$type]],
      rows_affected = result$rows_affected %||% NA,
      vars_affected = result$vars_affected %||% NA
    )
  }
  
  list(
    data = current_data,
    diagnostics = diagnostics,
    shadow_order = sapply(shadows, function(s) s$type)
  )
}

#' Visualize shadow dependencies
#'
#' @param shadows List of shadow objects (optional)
#' @return Prints dependency graph
#' @export
visualize_shadow_dependencies <- function(shadows = NULL) {
  deps <- get_shadow_dependencies()
  
  cat("Shadow Dependency Graph:\n")
  cat("=======================\n\n")
  
  for (shadow_type in names(deps)) {
    if (length(deps[[shadow_type]]) > 0) {
      cat(sprintf("%s depends on: %s\n", 
                  shadow_type, 
                  paste(deps[[shadow_type]], collapse = ", ")))
    } else {
      cat(sprintf("%s (no dependencies)\n", shadow_type))
    }
  }
  
  if (!is.null(shadows)) {
    cat("\nCurrent shadow ordering:\n")
    shadow_types <- sapply(shadows, function(s) s$type)
    cat(paste(seq_along(shadow_types), shadow_types, sep = ": ", collapse = "\n"))
    
    if (!check_shadow_ordering(shadows)) {
      cat("\n\nWARNING: Current ordering violates dependencies!\n")
      cat("Suggested ordering:\n")
      reordered <- reorder_shadows(shadows)
      reordered_types <- sapply(reordered, function(s) s$type)
      cat(paste(seq_along(reordered_types), reordered_types, sep = ": ", collapse = "\n"))
    }
  }
}