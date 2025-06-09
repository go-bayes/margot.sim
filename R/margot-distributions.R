#' Flexible Distribution Specifications for margot
#'
#' @description
#' This file implements a system for specifying custom distributions
#' in the data generating process, allowing for skewed, heavy-tailed,
#' and other non-normal distributions.

# Distribution Specification Objects -------------------------------------------

#' Create a distribution specification
#'
#' @param family Distribution family name or custom function
#' @param params Named list of distribution parameters
#' @param link Link function for transforming to linear predictor scale
#' @param inverse_link Inverse link function
#' @param name Optional name for the distribution
#'
#' @return Distribution specification object
#' @export
#'
#' @examples
#' # Normal distribution
#' dist_normal <- create_distribution("normal", params = list(sd = 1))
#'
#' # Log-normal via transformation
#' dist_lognormal <- create_distribution(
#'   "normal",
#'   params = list(sd = 0.5),
#'   inverse_link = exp
#' )
#'
#' # Beta distribution for bounded variables
#' dist_beta <- create_distribution(
#'   "beta",
#'   params = list(shape1 = 2, shape2 = 5)
#' )
create_distribution <- function(
    family,
    params = list(),
    link = NULL,
    inverse_link = NULL,
    name = NULL
) {

  # If family is a function, treat as custom
  if (is.function(family)) {
    dist_type <- "custom"
    generate_fn <- family
    family_name <- "custom"
  } else {
    dist_type <- "parametric"
    family_name <- family
    generate_fn <- NULL
  }

  # Set default link functions based on family
  if (is.null(link) && is.null(inverse_link)) {
    links <- get_default_links(family_name)
    link <- links$link
    inverse_link <- links$inverse_link
  }

  structure(
    list(
      type = dist_type,
      family = family_name,
      params = params,
      generate_fn = generate_fn,
      link = link,
      inverse_link = inverse_link,
      name = name %||% paste0(family_name, "_dist")
    ),
    class = c(paste0(dist_type, "_distribution"), "margot_distribution")
  )
}

#' Get default link functions for distribution families
#' @keywords internal
get_default_links <- function(family) {
  switch(family,
         normal = list(
           link = identity,
           inverse_link = identity
         ),
         binomial = list(
           link = qlogis,
           inverse_link = plogis
         ),
         poisson = list(
           link = log,
           inverse_link = exp
         ),
         gamma = list(
           link = log,
           inverse_link = exp
         ),
         beta = list(
           link = qlogis,
           inverse_link = plogis
         ),
         # Default to identity
         list(
           link = identity,
           inverse_link = identity
         )
  )
}

# Generation Functions ---------------------------------------------------------

#' Generate values from a distribution specification
#'
#' @param dist Distribution specification
#' @param n Number of values to generate
#' @param linear_predictor Optional linear predictor values
#'
#' @return Vector of generated values
#' @export
generate_from_dist <- function(dist, n, linear_predictor = NULL) {
  UseMethod("generate_from_dist", dist)
}

#' Generate from parametric distribution
#' @export
generate_from_dist.parametric_distribution <- function(dist, n, linear_predictor = NULL) {

  # If linear predictor provided, use it for location
  if (!is.null(linear_predictor)) {
    location <- dist$inverse_link(linear_predictor)
  } else {
    location <- 0
  }

  # Generate based on family
  values <- switch(dist$family,
                   normal = {
                     rnorm(n, mean = location, sd = dist$params$sd %||% 1)
                   },

                   lognormal = {
                     # Log-normal via exponential transformation
                     exp(rnorm(n, mean = dist$link(location), sd = dist$params$sdlog %||% 1))
                   },

                   binomial = {
                     size <- dist$params$size %||% 1
                     prob <- if (!is.null(linear_predictor)) {
                       dist$inverse_link(linear_predictor)
                     } else {
                       dist$params$prob %||% 0.5
                     }
                     rbinom(n, size = size, prob = prob)
                   },

                   poisson = {
                     lambda <- if (!is.null(linear_predictor)) {
                       dist$inverse_link(linear_predictor)
                     } else {
                       dist$params$lambda %||% 1
                     }
                     rpois(n, lambda = lambda)
                   },

                   gamma = {
                     shape <- dist$params$shape %||% 1
                     rate <- dist$params$rate %||% 1

                     if (!is.null(linear_predictor)) {
                       # Use linear predictor for mean
                       mean_val <- dist$inverse_link(linear_predictor)
                       rate <- shape / mean_val
                     }

                     rgamma(n, shape = shape, rate = rate)
                   },

                   beta = {
                     shape1 <- dist$params$shape1 %||% 1
                     shape2 <- dist$params$shape2 %||% 1

                     if (!is.null(linear_predictor)) {
                       # Use linear predictor for mean
                       mu <- dist$inverse_link(linear_predictor)
                       # Method of moments for beta parameters
                       phi <- dist$params$precision %||% (shape1 + shape2)
                       shape1 <- mu * phi
                       shape2 <- (1 - mu) * phi
                     }

                     rbeta(n, shape1 = shape1, shape2 = shape2)
                   },

                   t = {
                     df <- dist$params$df %||% 5
                     rt(n, df = df) * (dist$params$scale %||% 1) + location
                   },

                   uniform = {
                     min_val <- dist$params$min %||% 0
                     max_val <- dist$params$max %||% 1
                     runif(n, min = min_val, max = max_val)
                   },

                   exponential = {
                     rate <- dist$params$rate %||% 1
                     rexp(n, rate = rate)
                   },

                   # Default to normal
                   rnorm(n, mean = location, sd = dist$params$sd %||% 1)
  )

  values
}

#' Generate from custom distribution function
#' @export
generate_from_dist.custom_distribution <- function(dist, n, linear_predictor = NULL) {
  if (is.null(dist$generate_fn)) {
    stop("Custom distribution must have generate_fn")
  }

  # Call custom function with n and optional linear predictor
  if (!is.null(linear_predictor)) {
    dist$generate_fn(n, dist$inverse_link(linear_predictor), dist$params)
  } else {
    dist$generate_fn(n, dist$params)
  }
}

# Distribution Sets ------------------------------------------------------------

#' Create a set of distributions for different variable types
#'
#' @param baseline Distribution for baseline covariates
#' @param exposure Distribution for exposure/treatment
#' @param outcome Distribution for outcome
#' @param confounder Distribution for time-varying confounders
#' @param error Distribution for error terms
#'
#' @return Distribution set object
#' @export
create_distribution_set <- function(
    baseline = NULL,
    exposure = NULL,
    outcome = NULL,
    confounder = NULL,
    error = NULL
) {

  # Set defaults
  dists <- list(
    baseline = baseline %||% create_distribution("normal", params = list(sd = 1)),
    exposure = exposure %||% create_distribution("binomial", params = list(size = 1)),
    outcome = outcome %||% create_distribution("normal", params = list(sd = 1)),
    confounder = confounder %||% create_distribution("normal", params = list(sd = 1)),
    error = error %||% create_distribution("normal", params = list(sd = 1))
  )

  structure(dists, class = "margot_distribution_set")
}

# Integration with margot_simulate ---------------------------------------------

#' Enhanced margot_simulate with flexible distributions
#'
#' @inheritParams margot_simulate
#' @param distributions Distribution set or list of distributions
#'
#' @return Simulated data with specified distributions
#' @export
margot_simulate_flex <- function(
    n,
    waves,
    distributions = NULL,
    ...,
    validate_distributions = TRUE
) {

  # If no distributions specified, use standard margot_simulate
  if (is.null(distributions)) {
    return(margot_simulate(n = n, waves = waves, ...))
  }

  # Ensure we have a distribution set
  if (!inherits(distributions, "margot_distribution_set")) {
    distributions <- do.call(create_distribution_set, distributions)
  }

  # Extract other arguments
  args <- list(...)

  # Override the generation functions in margot_simulate
  # This would require modifying margot_simulate to accept custom generation functions
  # For now, we'll create a wrapper that generates the data with custom distributions

  # Generate baseline covariates with specified distribution
  n_baselines <- args$n_baselines %||% 5

  if (distributions$baseline$family == "normal") {
    # Standard multivariate normal
    b_cor <- 0.3
    sigma_b <- matrix(b_cor, n_baselines, n_baselines)
    diag(sigma_b) <- 1
    b_mat <- MASS::mvrnorm(n, rep(0, n_baselines), sigma_b)
  } else {
    # Generate independent baselines with specified distribution
    b_mat <- matrix(nrow = n, ncol = n_baselines)
    for (j in 1:n_baselines) {
      b_mat[, j] <- generate_from_dist(distributions$baseline, n)
    }
  }

  # Create initial data frame
  df <- data.frame(id = 1:n)
  colnames(b_mat) <- paste0("b", 1:n_baselines)
  df <- cbind(df, b_mat)

  # Continue with simulation using custom distributions...
  # This is a simplified example - full implementation would modify margot_simulate

  # For now, call standard margot_simulate and note this is a placeholder
  warning("Full distribution flexibility not yet implemented - using standard distributions")
  margot_simulate(n = n, waves = waves, ...)
}

# Utility Functions ------------------------------------------------------------

#' Check if a variable follows a specified distribution
#'
#' @param x Vector of values
#' @param dist Distribution specification or family name
#' @param plot Logical, create diagnostic plot?
#'
#' @return List with test results
#' @export
check_distribution <- function(x, dist, plot = TRUE) {
  x <- x[!is.na(x)]
  n <- length(x)

  # Get distribution family
  if (is.character(dist)) {
    family <- dist
  } else if (inherits(dist, "margot_distribution")) {
    family <- dist$family
  } else {
    stop("dist must be a distribution specification or family name")
  }

  # Perform appropriate test
  test_result <- switch(family,
                        normal = {
                          shapiro.test(x)
                        },

                        exponential = {
                          # Kolmogorov-Smirnov test
                          ks.test(x, "pexp", rate = 1/mean(x))
                        },

                        uniform = {
                          ks.test(x, "punif", min = min(x), max = max(x))
                        },

                        # Default: just return summary stats
                        list(
                          statistic = NA,
                          p.value = NA,
                          method = "No specific test available"
                        )
  )

  # Create diagnostic plot if requested
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- create_distribution_diagnostic_plot(x, family)
    print(p)
  }

  list(
    test = test_result,
    summary = summary(x),
    skewness = moments::skewness(x),
    kurtosis = moments::kurtosis(x)
  )
}

#' Create diagnostic plot for distribution
#' @keywords internal
create_distribution_diagnostic_plot <- function(x, family) {
  df <- data.frame(x = x)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                            bins = 30, fill = "lightblue", alpha = 0.7) +
    ggplot2::theme_minimal()

  # Add theoretical density
  if (family == "normal") {
    p <- p + ggplot2::stat_function(
      fun = dnorm,
      args = list(mean = mean(x), sd = sd(x)),
      color = "red",
      size = 1
    )
  } else if (family == "exponential") {
    p <- p + ggplot2::stat_function(
      fun = dexp,
      args = list(rate = 1/mean(x)),
      color = "red",
      size = 1
    )
  }

  p + ggplot2::labs(
    title = paste("Distribution Check:", family),
    x = "Value",
    y = "Density"
  )
}

# Example Distributions --------------------------------------------------------

#' Common distribution specifications
#' @export
margot_distributions <- list(
  # Standard distributions
  normal = create_distribution("normal", params = list(sd = 1)),

  binary = create_distribution("binomial", params = list(size = 1)),

  count = create_distribution("poisson", params = list(lambda = 2)),

  # Skewed distributions
  lognormal = create_distribution(
    "normal",
    params = list(sd = 0.5),
    inverse_link = exp,
    name = "lognormal"
  ),

  gamma_skewed = create_distribution(
    "gamma",
    params = list(shape = 2, rate = 1)
  ),

  # Heavy-tailed
  t_heavy = create_distribution(
    "t",
    params = list(df = 3, scale = 1)
  ),

  # Bounded distributions
  beta_bounded = create_distribution(
    "beta",
    params = list(shape1 = 2, shape2 = 5)
  ),

  uniform_bounded = create_distribution(
    "uniform",
    params = list(min = 0, max = 1)
  ),

  # Zero-inflated (custom)
  zero_inflated_normal = create_distribution(
    family = function(n, location = 0, params) {
      # Probability of zero
      p_zero <- params$p_zero %||% 0.3

      # Generate zeros
      n_zeros <- rbinom(1, n, p_zero)
      n_nonzero <- n - n_zeros

      # Generate non-zero values
      values <- numeric(n)
      if (n_nonzero > 0) {
        values[1:n_nonzero] <- rnorm(n_nonzero,
                                     mean = location,
                                     sd = params$sd %||% 1)
      }

      # Shuffle
      sample(values)
    },
    params = list(p_zero = 0.3, sd = 1),
    name = "zero_inflated_normal"
  )
)

# Example Usage ----------------------------------------------------------------

#' Example: Simulate with different distributions
#' @export
example_flexible_distributions <- function() {
  cat("Example: Flexible Distributions\n")
  cat("===============================\n\n")

  # Create distribution set with skewed distributions
  dists <- create_distribution_set(
    baseline = margot_distributions$gamma_skewed,
    exposure = margot_distributions$binary,
    outcome = margot_distributions$lognormal,
    confounder = margot_distributions$t_heavy
  )

  # Note: Full integration pending
  cat("Distribution set created:\n")
  cat("- Baseline: Gamma (right-skewed)\n")
  cat("- Exposure: Binary\n")
  cat("- Outcome: Log-normal\n")
  cat("- Confounder: t-distribution (heavy tails)\n\n")

  # Demonstrate distribution generation
  cat("Sample generation from each distribution:\n\n")

  # Baseline
  baseline_vals <- generate_from_dist(dists$baseline, 1000)
  cat(sprintf("Baseline: mean = %.2f, sd = %.2f, skew = %.2f\n",
              mean(baseline_vals), sd(baseline_vals),
              moments::skewness(baseline_vals)))

  # Outcome with linear predictor
  lp <- rnorm(1000, mean = 1, sd = 0.5)
  outcome_vals <- generate_from_dist(dists$outcome, 1000, linear_predictor = lp)
  cat(sprintf("Outcome: mean = %.2f, sd = %.2f, skew = %.2f\n",
              mean(outcome_vals), sd(outcome_vals),
              moments::skewness(outcome_vals)))

  # Check distribution
  cat("\nDistribution check for outcome:\n")
  check_result <- check_distribution(log(outcome_vals), "normal", plot = FALSE)
  cat(sprintf("Log-transformed normality test p-value: %.3f\n",
              check_result$test$p.value))

  invisible(list(distributions = dists, samples = list(
    baseline = baseline_vals,
    outcome = outcome_vals
  )))
}
