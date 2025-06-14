#' Optional Dependencies for margot.sim
#'
#' @description
#' Functions to check and use optional dependencies that are in Suggests
#' rather than Imports. This keeps the core package lightweight.

#' Check if a suggested package is available
#'
#' @param package Character string of package name
#' @param stop If TRUE, stop with error if not available
#' @param message Custom message to show if package not available
#' @return Logical indicating if package is available
#' @keywords internal
check_suggests <- function(package, stop = FALSE, message = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    if (is.null(message)) {
      message <- sprintf(
        "Package '%s' is required for this functionality but not installed.\n",
        package
      )
      
      # add installation instructions based on package
      if (package %in% c("arrow", "dagitty", "igraph", "ggdag")) {
        message <- paste0(
          message,
          sprintf("Install it with: install.packages('%s')", package)
        )
      }
    }
    
    if (stop) {
      stop(message, call. = FALSE)
    } else {
      warning(message, call. = FALSE)
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Use arrow for large data operations
#'
#' @param expr Expression to evaluate with arrow
#' @return Result of expression or NULL if arrow not available
#' @keywords internal
with_arrow <- function(expr) {
  if (check_suggests("arrow")) {
    eval(expr)
  } else {
    NULL
  }
}

#' Use igraph for graph operations
#'
#' @param expr Expression to evaluate with igraph
#' @return Result of expression or NULL if igraph not available
#' @keywords internal
with_igraph <- function(expr) {
  if (check_suggests("igraph")) {
    eval(expr)
  } else {
    NULL
  }
}

#' Check all optional dependencies
#'
#' @return Data frame with status of optional packages
#' @export
#' @examples
#' \dontrun{
#' check_margot_suggests()
#' }
check_margot_suggests <- function() {
  optional <- margot_optional_deps()
  
  optional$installed <- vapply(
    optional$package,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )
  
  optional$version <- ifelse(
    optional$installed,
    vapply(optional$package, function(pkg) {
      as.character(utils::packageVersion(pkg))
    }, character(1)),
    NA_character_
  )
  
  class(optional) <- c("margot_suggests", "data.frame")
  optional
}

#' Print method for margot_suggests
#' 
#' @param x A margot_suggests object
#' @param ... Additional arguments (unused)
#' @export
print.margot_suggests <- function(x, ...) {
  cat("margot.sim Optional Dependencies\n")
  cat("================================\n\n")
  
  installed <- x[x$installed, ]
  not_installed <- x[!x$installed, ]
  
  if (nrow(installed) > 0) {
    cat("Installed:\n")
    for (i in seq_len(nrow(installed))) {
      cat(sprintf("  [OK] %s (%s): %s\n", 
                  installed$package[i],
                  installed$version[i],
                  installed$reason[i]))
    }
  }
  
  if (nrow(not_installed) > 0) {
    cat("\nNot installed:\n")
    for (i in seq_len(nrow(not_installed))) {
      cat(sprintf("  [X] %s: %s\n",
                  not_installed$package[i],
                  not_installed$reason[i]))
    }
    
    cat("\nTo install missing packages:\n")
    cat(sprintf("  install.packages(c(%s))\n",
                paste0('"', not_installed$package, '"', collapse = ", ")))
  }
  
  invisible(x)
}

#' Get heavy dependency usage in current session
#'
#' @return List of loaded optional packages
#' @export
margot_loaded_suggests <- function() {
  optional <- margot_optional_deps()$package
  loaded <- intersect(optional, loadedNamespaces())
  
  if (length(loaded) == 0) {
    message("No optional packages currently loaded")
  } else {
    message("Loaded optional packages: ", paste(loaded, collapse = ", "))
  }
  
  invisible(loaded)
}