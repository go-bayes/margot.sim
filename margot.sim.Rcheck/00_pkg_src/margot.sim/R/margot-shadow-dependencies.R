#' Shadow Dependency Management System
#'
#' @description
#' Implements a dependency system for shadows to handle interactions
#' between different types of observational distortions. For example,
#' truncation changes the variance which affects measurement error.
#'
#' @details
#' The system uses a directed acyclic graph (DAG) to represent dependencies
#' and automatically reorders shadows for correct application sequence.

# dependency definitions ----------------------------------------------

#' Get shadow dependency definitions
#'
#' @description
#' Returns the dependency relationships between shadow types.
#' Each shadow type can declare which aspects of the data it modifies
#' and which aspects it depends on.
#'
#' @return List of dependency definitions
#' @keywords internal
get_shadow_dependencies <- function() {
  list(
    measurement_error = list(
      depends_on = c("distribution"),  # needs accurate variance
      modifies = c("values"),
      priority = 5  # higher number = apply later
    ),
    
    truncation = list(
      depends_on = c(),  # no dependencies
      modifies = c("distribution", "sample_size"),
      priority = 1  # apply early
    ),
    
    censoring = list(
      depends_on = c(),
      modifies = c("values", "completeness"),
      priority = 2
    ),
    
    selection = list(
      depends_on = c("values"),  # depends on variable values
      modifies = c("sample_size"),
      priority = 3
    ),
    
    positivity = list(
      depends_on = c("values"),
      modifies = c("sample_size"),
      priority = 3
    ),
    
    missing_data = list(
      depends_on = c("values"),
      modifies = c("completeness"),
      priority = 4
    ),
    
    item_missingness = list(
      depends_on = c("values"),
      modifies = c("completeness"),
      priority = 4
    ),
    
    coarsening = list(
      depends_on = c("values"),
      modifies = c("values", "precision"),
      priority = 6
    ),
    
    mode_effects = list(
      depends_on = c("values"),
      modifies = c("values"),
      priority = 6
    ),
    
    misclassification = list(
      depends_on = c("values"),
      modifies = c("values"),
      priority = 6
    )
  )
}

#' Check shadow ordering for dependencies
#'
#' @description
#' Verifies that shadows are ordered correctly based on their dependencies.
#'
#' @param shadows List of shadow objects
#' @return List with validation results
#' @export
check_shadow_ordering <- function(shadows) {
  if (length(shadows) <= 1) {
    return(list(
      valid = TRUE,
      issues = character(0),
      suggested_order = shadows
    ))
  }
  
  deps <- get_shadow_dependencies()
  
  # extract shadow types
  shadow_types <- sapply(shadows, function(s) {
    if (inherits(s, "margot_shadow")) {
      s$type
    } else if (is.list(s) && !is.null(s$type)) {
      s$type
    } else {
      "unknown"
    }
  })
  
  issues <- character(0)
  
  # check each shadow's dependencies
  for (i in seq_along(shadows)) {
    current_type <- shadow_types[i]
    if (current_type == "unknown") next
    
    current_deps <- deps[[current_type]]
    if (is.null(current_deps)) next
    
    # check what this shadow depends on
    depends_on <- current_deps$depends_on
    
    # check if any earlier shadows modify what this depends on
    if (i > 1 && length(depends_on) > 0) {
      for (j in 1:(i-1)) {
        earlier_type <- shadow_types[j]
        if (earlier_type == "unknown") next
        
        earlier_deps <- deps[[earlier_type]]
        if (is.null(earlier_deps)) next
        
        # check if earlier shadow modifies what current shadow depends on
        if (any(depends_on %in% earlier_deps$modifies)) {
          # this is good - dependency is satisfied
          next
        }
      }
      
      # now check if any dependency is NOT satisfied by earlier shadows
      for (dep in depends_on) {
        satisfied <- FALSE
        for (j in 1:(i-1)) {
          earlier_type <- shadow_types[j]
          if (earlier_type == "unknown") next
          
          earlier_deps <- deps[[earlier_type]]
          if (!is.null(earlier_deps) && dep %in% earlier_deps$modifies) {
            satisfied <- TRUE
            break
          }
        }
        
        if (!satisfied) {
          # check if any later shadow provides this dependency
          for (j in (i+1):length(shadows)) {
            if (j > length(shadows)) break
            later_type <- shadow_types[j]
            if (later_type == "unknown") next
            
            later_deps <- deps[[later_type]]
            if (!is.null(later_deps) && dep %in% later_deps$modifies) {
              issues <- c(issues, sprintf(
                "%s shadow at position %d depends on '%s', which is modified by %s shadow at position %d (should come earlier)",
                current_type, i, dep, later_type, j
              ))
              break
            }
          }
        }
      }
    }
  }
  
  # suggest reordering if issues found
  suggested_order <- if (length(issues) > 0) {
    reorder_shadows(shadows)
  } else {
    shadows
  }
  
  list(
    valid = length(issues) == 0,
    issues = issues,
    suggested_order = suggested_order
  )
}

#' Reorder shadows based on dependencies
#'
#' @description
#' Automatically reorders shadows to respect dependency relationships.
#' Uses topological sorting based on the dependency DAG.
#'
#' @param shadows List of shadow objects
#' @return Reordered list of shadows
#' @export
reorder_shadows <- function(shadows) {
  if (length(shadows) <= 1) return(shadows)
  
  deps <- get_shadow_dependencies()
  
  # extract shadow info
  shadow_info <- lapply(seq_along(shadows), function(i) {
    s <- shadows[[i]]
    type <- if (inherits(s, "margot_shadow")) {
      s$type
    } else if (is.list(s) && !is.null(s$type)) {
      s$type
    } else {
      "unknown"
    }
    
    priority <- if (!is.null(deps[[type]]$priority)) {
      deps[[type]]$priority
    } else {
      5  # default middle priority
    }
    
    list(
      index = i,
      shadow = s,
      type = type,
      priority = priority
    )
  })
  
  # sort by priority (lower priority = apply earlier)
  sorted_info <- shadow_info[order(sapply(shadow_info, function(x) x$priority))]
  
  # extract reordered shadows
  lapply(sorted_info, function(x) x$shadow)
}

#' Update shadow parameters based on upstream shadows
#'
#' @description
#' Adjusts shadow parameters based on the effects of previously applied shadows.
#' For example, adjusts measurement error variance after truncation.
#'
#' @param shadow Shadow object to update
#' @param upstream_effects List describing effects of upstream shadows
#' @return Updated shadow object
#' @export
update_shadow_params <- function(shadow, upstream_effects) {
  if (length(upstream_effects) == 0) return(shadow)
  
  shadow_type <- if (inherits(shadow, "margot_shadow")) {
    shadow$type
  } else {
    shadow$type
  }
  
  # handle specific updates based on shadow type
  if (shadow_type == "measurement_error" && !is.null(upstream_effects$truncation)) {
    # adjust variance for truncation
    truncation_info <- upstream_effects$truncation
    
    if (!is.null(truncation_info$variance_reduction)) {
      # reduce measurement error variance proportionally
      if (!is.null(shadow$params$sigma)) {
        shadow$params$sigma <- shadow$params$sigma * 
          sqrt(truncation_info$variance_reduction)
      }
    }
  }
  
  if (shadow_type == "missing_data" && !is.null(upstream_effects$selection)) {
    # adjust missing data probability based on selection
    selection_info <- upstream_effects$selection
    
    if (!is.null(selection_info$retention_rate)) {
      # increase missing data rate in retained sample
      if (!is.null(shadow$params$prob)) {
        # adjust probability considering selection
        shadow$params$prob <- 1 - (1 - shadow$params$prob) * 
          selection_info$retention_rate
      }
    }
  }
  
  shadow
}

#' Apply shadows with dependency management
#'
#' @description
#' Applies a list of shadows to data while respecting dependencies
#' and updating parameters as needed.
#'
#' @param data Data frame to apply shadows to
#' @param shadows List of shadow objects
#' @param check_dependencies Whether to check and reorder based on dependencies
#' @param verbose Print progress messages
#' @return Data with shadows applied
#' @export
#' @examples
#' \dontrun{
#' # shadows will be automatically reordered
#' shadows <- list(
#'   create_shadow("measurement_error", list(
#'     variables = "y",
#'     error_type = "classical",
#'     sigma = 0.5
#'   )),
#'   create_shadow("truncation", list(
#'     variables = "y",
#'     lower = -2,
#'     upper = 2
#'   ))
#' )
#' 
#' data_shadowed <- apply_shadows_with_dependencies(data, shadows)
#' }
apply_shadows_with_dependencies <- function(
    data, 
    shadows, 
    check_dependencies = TRUE,
    verbose = FALSE
) {
  if (length(shadows) == 0) return(data)
  
  # check and reorder if requested
  if (check_dependencies) {
    ordering_result <- check_shadow_ordering(shadows)
    
    if (!ordering_result$valid) {
      if (verbose) {
        message("Shadow dependency issues detected:")
        for (issue in ordering_result$issues) {
          message("  - ", issue)
        }
        message("Reordering shadows automatically...")
      }
      shadows <- ordering_result$suggested_order
    }
  }
  
  # track upstream effects
  upstream_effects <- list()
  data_shadowed <- data
  
  # apply each shadow
  for (i in seq_along(shadows)) {
    shadow <- shadows[[i]]
    
    # update shadow parameters based on upstream effects
    shadow <- update_shadow_params(shadow, upstream_effects)
    
    if (verbose) {
      shadow_type <- if (inherits(shadow, "margot_shadow")) {
        shadow$type
      } else {
        shadow$type
      }
      message(sprintf("Applying shadow %d/%d: %s", i, length(shadows), shadow_type))
    }
    
    # apply shadow
    data_shadowed <- apply_shadow(data_shadowed, shadow)
    
    # track effects for downstream shadows
    shadow_type <- if (inherits(shadow, "margot_shadow")) {
      shadow$type
    } else {
      shadow$type
    }
    
    # record specific effects based on shadow type
    if (shadow_type == "truncation") {
      # calculate variance reduction from truncation
      truncated_vars <- shadow$params$variables
      if (!is.null(truncated_vars)) {
        for (var in truncated_vars) {
          if (var %in% names(data) && var %in% names(data_shadowed)) {
            var_before <- var(data[[var]], na.rm = TRUE)
            var_after <- var(data_shadowed[[var]], na.rm = TRUE)
            
            upstream_effects$truncation <- list(
              variables = truncated_vars,
              variance_reduction = var_after / var_before
            )
          }
        }
      }
    }
    
    if (shadow_type == "selection") {
      # record retention rate
      upstream_effects$selection <- list(
        retention_rate = nrow(data_shadowed) / nrow(data)
      )
    }
  }
  
  # add attributes about shadow application
  attr(data_shadowed, "shadows_applied") <- TRUE
  attr(data_shadowed, "shadow_order") <- sapply(shadows, function(s) {
    if (inherits(s, "margot_shadow")) s$type else s$type
  })
  attr(data_shadowed, "upstream_effects") <- upstream_effects
  
  data_shadowed
}

#' Visualize shadow dependencies
#'
#' @description
#' Creates a visualization of shadow dependencies as a directed graph.
#'
#' @param shadows List of shadow objects (optional)
#' @param show_all Logical. Show all possible shadow types and dependencies
#' @return A plot object (if igraph is available) or a text representation
#' @export
visualize_shadow_dependencies <- function(shadows = NULL, show_all = FALSE) {
  deps <- get_shadow_dependencies()
  
  if (show_all) {
    # show all possible dependencies
    shadow_types <- names(deps)
  } else if (!is.null(shadows)) {
    # show only provided shadows
    shadow_types <- sapply(shadows, function(s) {
      if (inherits(s, "margot_shadow")) s$type else s$type
    })
  } else {
    message("No shadows provided and show_all = FALSE. Nothing to visualize.")
    return(invisible(NULL))
  }
  
  # create edge list
  edges <- list()
  for (shadow_type in shadow_types) {
    dep_info <- deps[[shadow_type]]
    if (!is.null(dep_info)) {
      # what this shadow modifies
      modifies <- dep_info$modifies
      
      # check which other shadows depend on these modifications
      for (other_type in shadow_types) {
        if (other_type == shadow_type) next
        
        other_deps <- deps[[other_type]]
        if (!is.null(other_deps)) {
          # does other shadow depend on what this shadow modifies?
          if (any(other_deps$depends_on %in% modifies)) {
            edges[[length(edges) + 1]] <- c(shadow_type, other_type)
          }
        }
      }
    }
  }
  
  # try to create graph visualization
  if (requireNamespace("igraph", quietly = TRUE)) {
    if (length(edges) > 0) {
      edge_matrix <- do.call(rbind, edges)
      g <- igraph::graph_from_edgelist(edge_matrix, directed = TRUE)
      
      # add isolated nodes
      all_nodes <- unique(shadow_types)
      existing_nodes <- unique(c(edge_matrix))
      isolated <- setdiff(all_nodes, existing_nodes)
      if (length(isolated) > 0) {
        g <- igraph::add_vertices(g, length(isolated), name = isolated)
      }
      
      # set node colors based on priority
      node_names <- igraph::V(g)$name
      priorities <- sapply(node_names, function(n) {
        if (!is.null(deps[[n]]$priority)) deps[[n]]$priority else 5
      })
      
      # color scheme: early (blue) to late (red)
      colors <- grDevices::colorRampPalette(c("lightblue", "yellow", "orange", "red"))(7)
      node_colors <- colors[priorities]
      
      # plot
      igraph::plot.igraph(g, 
           vertex.color = node_colors,
           vertex.size = 30,
           vertex.label.cex = 0.8,
           edge.arrow.size = 0.5,
           layout = igraph::layout_with_sugiyama(g)$layout,
           main = "Shadow Dependencies\n(Arrow: must be applied before)")
      
      # add legend
      graphics::legend("bottomright", 
             legend = paste("Priority", 1:7),
             fill = colors,
             cex = 0.7,
             title = "Application Order")
      
      return(invisible(g))
    } else {
      message("No dependencies found between shadows.")
    }
  } else {
    # text representation
    cat("Shadow Dependencies (text representation):\n")
    cat("==========================================\n")
    
    for (shadow_type in shadow_types) {
      dep_info <- deps[[shadow_type]]
      if (!is.null(dep_info)) {
        cat(sprintf("\n%s (priority: %d):\n", 
                   shadow_type, 
                   dep_info$priority %||% 5))
        cat(sprintf("  Depends on: %s\n", 
                   paste(dep_info$depends_on, collapse = ", ")))
        cat(sprintf("  Modifies: %s\n", 
                   paste(dep_info$modifies, collapse = ", ")))
      }
    }
    
    if (length(edges) > 0) {
      cat("\nApplication order constraints:\n")
      for (edge in edges) {
        cat(sprintf("  %s -> %s\n", edge[1], edge[2]))
      }
    }
  }
}