#' Simulate semi-markovian longitudinal data with sampling weights
#'
#' @description
#' Generates data following a semi-markovian structural causal model (SCM) with:
#' - Semi-markovian data generating process
#' - Optional sampling weights to reflect target population
#' - Intervention functions for causal inference
#' - Post-hoc censoring for bias analysis
#'
#' The function cleanly separates three stages:
#' 1. Data generation under intervention g (if specified): \eqn{(B, L_k^g, A_k^g, Y_k^g)}
#' 2. Application of sampling weights to match target population
#' 3. Creation of censoring indicators: \eqn{C_k} (via margot_process_longitudinal)
#'
#' This mirrors counterfactual notation: \eqn{A_k \to A_k^g, C_k \equiv 0, Y_K^{g,C=0}}
#'
#' @section Directed Acyclic Graph (DAG):
#' The data generating process follows this DAG structure:
#' \preformatted{
#'     B --+---> L_1 ---> A_1 ---> Y_1 ---> L_2 ---> A_2 ---> Y_2 ---> ... ---> Y_K
#'         |      |      |      |      |      |      |
#'         |      v      v      v      v      v      v
#'         +----> C_1     C_2     C_3    C_4     C_5     C_6
#'
#' Where:
#'   B  = Baseline covariates (time-invariant)
#'   L_k = Time-varying confounders at wave k
#'   A_k = Exposure/treatment at wave k
#'   Y_k = Outcome(s) at wave k
#'   C_k = Censoring indicator after wave k
#' }
#'
#' @section Structural Equations:
#' The SCM is defined by the following structural equations.
#'
#' For baseline (k=0):
#' \deqn{B \sim \text{MVN}(0, \Sigma_B) \text{ with } \Sigma_B[i,j] = \rho_B \text{ if } i \neq j}
#' \deqn{A_0 = f_{A_0}(B, U_{A_0})}
#' \deqn{Y_0 = f_{Y_0}(B, A_0, U_{Y_0})}
#'
#' For wave k >= 1:
#' \deqn{L_k = \beta_{B \to L} \cdot h_k(B) + \beta_{A \to L} A_{k-1} + \beta_{Y \to L} Y_{k-1} + U_{L_k}}
#' \deqn{A_k = f_{A_k}(B, L_k, A_{k-1}, Y_{k-1}, U_{A_k})}
#' \deqn{Y_k = f_{Y_k}(B, L_{k-1}, A_{k-1}, Y_{k-1}, U_{Y_k})}
#' \deqn{C_k = f_{C_k}(A_{k-1}, L_{k-1}, Y_{k-1}, \theta, U_{C_k})}
#'
#' Where:
#' - \eqn{h_k(B)} represents time-varying functions of baseline covariates
#' - \eqn{U_{\cdot} \sim N(0,1)} are independent errors except as noted below
#' - \eqn{\theta} is a shared frailty term when latent_dependence = TRUE
#'
#' @section Unmeasured Confounding:
#' The following unmeasured confounding structures are supported:
#'
#' 1. **Independent errors** (default): All \eqn{U_{\cdot}} are mutually independent
#'
#' 2. **Shared frailty for censoring**: When \code{latent_dependence = TRUE},
#'    \deqn{\theta \sim N(0, \sigma^2_{\text{frailty}})}
#'    affects all censoring events, inducing correlation in dropout times
#'
#' 3. **Correlated outcomes**: When \code{n_outcomes > 1},
#'    \deqn{U_{Y_k} \sim \text{MVN}(0, \Sigma_Y)}
#'    where \eqn{\Sigma_Y} has off-diagonal elements \code{y_cor}
#'
#' Note: There is no unmeasured confounding between treatment and outcome by design.
#' All confounding passes through measured variables (B, L).
#'
#' @section Parameter Interpretation:
#' Key parameters in the structural equations:
#' - \code{b_l_coef}: \eqn{\beta_{B \to L}} effect of baseline on time-varying confounder
#' - \code{a_l_coef}: \eqn{\beta_{A \to L}} effect of past treatment on confounder
#' - \code{y_l_coef}: \eqn{\beta_{Y \to L}} feedback from past outcome to confounder
#' - \code{l_a_coef}: \eqn{\beta_{L \to A}} effect of confounder on treatment
#' - \code{a_autoreg}: \eqn{\beta_{A \to A}} treatment persistence
#' - \code{a_lag_y_coef}: \eqn{\beta_{A \to Y}} causal effect of treatment on outcome
#' - \code{cens_*_coef}: Effects on censoring hazard
#'
#' @param n Integer. Number of subjects to simulate
#' @param waves Integer. Number of measurement waves (time points)
#' @param n_outcomes Integer. Number of outcomes (1-3). Default: 1
#' @param n_baselines Integer. Number of baseline covariates. Default: 5
#' @param exposure_type Character. Type of exposure: "binary" or "continuous". Default: "binary"
#' @param outcome_type Character. Type of outcome: "continuous" or "binary". Default: "continuous"
#' @param y_feedback Character. Type of outcome feedback: "full", "y_only", or "none". Default: "full"
#' @param censoring List. Censoring parameters including rate, exposure_dependence, l_dependence, y_dependence, latent_dependence
#' @param params List. Named list of model parameters (see defaults in .default_sim_params())
#' @param seed Integer. Random seed for reproducibility
#' @param wide Logical. Return data in wide format? Default: TRUE
#' @param validate_props Logical. Validate that coefficients sum to < 1? Default: TRUE
#' @param verbose Logical. Print progress messages? Default: FALSE
#' @param intervention Function. Intervention function(data, time, trt) that modifies treatment
#' @param sampling_weights Numeric vector of length n, or function(baseline_data)
#'   that returns weights. Used to weight baseline covariates to reflect
#'   target population. If NULL, no weighting is applied.
#' @param apply_process_function Logical. If TRUE, applies
#'   margot_process_longitudinal to create proper censoring indicators
#' @param process_args List of arguments to pass to margot_process_longitudinal
#' @param shadows A shadow object or list of shadow objects created with
#'   \code{create_shadow()} or related functions. Shadows are applied after data
#'   generation and processing to simulate observational distortions like
#'   measurement error, missingness, or selection bias.
#'
#' @return A tibble with columns for:
#'   - \code{id}: Subject identifier
#'   - \code{b1, ..., bn}: Baseline covariates
#'   - \code{t0_a, t1_a, ...}: Treatment assignments (natural or under intervention)
#'   - \code{t1_l, t2_l, ...}: Time-varying confounders
#'   - \code{t0_y, t1_y, ..., t(K+1)_y}: Outcomes
#'   - \code{t*_not_lost_following_wave}: Censoring indicators (if apply_process_function = TRUE)
#'   - \code{sampling_weight}: Applied weights (if sampling_weights provided)
#'
#' The returned object has attribute "margot_meta" containing simulation parameters.
#'
#' @examples
#' # basic simulation following the SCM
#' dat <- margot_simulate(
#'   n = 1000,
#'   waves = 3,
#'   seed = 2025
#' )
#'
#' # with intervention g: always treat after baseline
#' dat_g <- margot_simulate(
#'   n = 1000,
#'   waves = 3,
#'   intervention = function(data, time, trt) {
#'     if (time == 0) return(data[[trt]])  # natural at baseline
#'     rep(1, nrow(data))                   # always treat after
#'   },
#'   seed = 2025
#' )
#'
#' # the estimand E[Y_K^{g,C=0}] can be computed as mean(dat_g$t4_y)
#'
#' # Example with shadows
#' # Create measurement error shadow
#' me_shadow <- create_shadow(
#'   "measurement_error",
#'   params = list(
#'     variables = c("t1_l", "t2_l"),
#'     error_type = "classical",
#'     sigma = 0.5
#'   )
#' )
#'
#' # Generate data with measurement error
#' dat_with_error <- margot_simulate(
#'   n = 1000,
#'   waves = 2,
#'   shadows = me_shadow,
#'   seed = 2025
#' )
#' # shadowed data has measurement error applied to specified variables
#'
#' @references
#' Robins JM (1986). "A new approach to causal inference in mortality studies
#' with sustained exposure periods--application to control of the healthy worker
#' survivor effect." Mathematical Modelling, 7(9-12), 1393-1512.
#'
#' Hernan MA, Robins JM (2020). "Causal Inference: What If."
#' Boca Raton: Chapman & Hall/CRC.
#'
#' @export
margot_simulate <- function(
    n,
    waves,
    n_outcomes     = 1,
    n_baselines    = 5,
    exposure_type  = "binary",
    outcome_type   = "continuous",
    y_feedback     = c("full", "y_only", "none"),
    censoring      = list(
      rate = 0.1,
      exposure_dependence = TRUE,
      l_dependence = FALSE,
      y_dependence = FALSE,
      latent_dependence = FALSE
    ),
    params         = list(),
    seed           = NULL,
    wide           = TRUE,
    validate_props = TRUE,
    verbose        = FALSE,
    intervention   = NULL,
    sampling_weights = NULL,
    apply_process_function = TRUE,  # changed default to TRUE
    process_args = list(),
    shadows = NULL) {

  # setup
  if (!is.null(seed)) set.seed(seed)

  # input validation
  if (n < 1) stop("n must be positive")
  if (waves < 1) stop("waves must be positive")
  if (n_baselines < 1) stop("n_baselines must be at least 1")

  y_feedback <- match.arg(y_feedback)
  n_outcomes <- min(max(n_outcomes, 1), 3)

  # merge parameters
  p <- modifyList(.default_sim_params(), params)

  # merge censoring defaults
  censoring <- modifyList(
    list(
      rate = 0.1,
      exposure_dependence = TRUE,
      l_dependence = FALSE,
      y_dependence = FALSE,
      latent_dependence = FALSE
    ),
    censoring
  )

  # helpers
  expit <- function(x) {
    x <- pmax(-20, pmin(20, x))
    1 / (1 + exp(-x))
  }

  rbern <- function(n, p) {
    if (any(!is.na(p))) {
      p[!is.na(p)] <- pmax(0, pmin(1, p[!is.na(p)]))
    }
    suppressWarnings(rbinom(n, 1, p))
  }

  # determine what to generate
  generate_y <- y_feedback != "none"
  use_y_feedback <- y_feedback == "full"

  # initialize storage
  df <- tibble::tibble(id = seq_len(n))

  # generate baseline covariates with optional weighting
  b_cor <- 0.3
  sigma_b <- matrix(b_cor, n_baselines, n_baselines)
  diag(sigma_b) <- 1

  # generate initial baseline covariates
  b_mat <- MASS::mvrnorm(n, rep(0, n_baselines), sigma_b)

  # apply sampling weights if provided
  if (!is.null(sampling_weights)) {
    if (is.function(sampling_weights)) {
      # compute weights from baseline data
      temp_df <- as.data.frame(b_mat)
      colnames(temp_df) <- paste0("b", seq_len(n_baselines))
      weights <- sampling_weights(temp_df)
    } else if (length(sampling_weights) == n) {
      weights <- sampling_weights
    } else {
      stop("sampling_weights must be a vector of length n or a function")
    }

    # normalize weights
    weights <- weights / mean(weights)

    # resample baseline covariates according to weights
    # this creates a weighted sample that reflects the target population
    idx <- sample(n, n, replace = TRUE, prob = weights)
    b_mat <- b_mat[idx, ]

    # store weights for later use
    df$sampling_weight <- weights[idx]
  }

  colnames(b_mat) <- paste0("b", seq_len(n_baselines))
  df <- dplyr::bind_cols(df, as.data.frame(b_mat))

  # ---- generate complete data (no censoring) ----

  # tracking for censoring simulation (but don't apply yet)
  latent_frailty <- if (censoring$latent_dependence) rnorm(n) else rep(0, n)
  censoring_probs <- list()  # store censoring probabilities for later

  # generate t0 exposure
  if (exposure_type == "binary") {
    lp_a0 <- p$b_a_coef * b_mat[, 1]
    natural_a0 <- rbern(n, expit(lp_a0))
  } else {
    natural_a0 <- p$b_a_coef * b_mat[, 1] + rnorm(n)
  }

  # apply intervention at t0 if specified
  if (!is.null(intervention)) {
    df$t0_a <- intervention(
      cbind(df, t0_a = natural_a0),
      time = 0,
      trt = "t0_a"
    )
  } else {
    df$t0_a <- natural_a0
  }

  # generate t0 outcomes if needed
  if (generate_y) {
    sigma_y <- matrix(p$y_cor, n_outcomes, n_outcomes)
    diag(sigma_y) <- 1
    eps_y0 <- MASS::mvrnorm(n, rep(0, n_outcomes), sigma_y)
    if (n_outcomes == 1) eps_y0 <- matrix(eps_y0, ncol = 1)

    for (j in seq_len(n_outcomes)) {
      shrink <- if (j == 1) 1 else p[[paste0("y", j, "_shrink")]]
      mu_y0 <- shrink * (p$a_lag_y_coef * df$t0_a + p$b_y_coef * b_mat[, 1])

      if (outcome_type == "continuous") {
        df[[paste0("t0_y", j)]] <- mu_y0 + eps_y0[, j]
      } else {
        df[[paste0("t0_y", j)]] <- rbern(n, expit(mu_y0 + eps_y0[, j]))
      }
    }
  }

  # calculate t0->t1 censoring probability (but don't apply)
  if (censoring$rate > 0) {
    cens_lp <- rep(qlogis(censoring$rate), n)

    if (censoring$exposure_dependence) {
      cens_lp <- cens_lp + p$cens_a_coef * df$t0_a
    }

    if (isTRUE(censoring$y_dependence) && generate_y) {
      cens_lp <- cens_lp + p$cens_y_coef * df$t0_y1
    }

    if (censoring$latent_dependence) {
      cens_lp <- cens_lp + p$cens_latent_sd * latent_frailty
    }

    censoring_probs[[1]] <- expit(cens_lp)
  }

  # main time loop - generate complete data
  for (t in seq_len(waves)) {
    if (verbose && waves > 10 && t %% 10 == 0) {
      message("  wave ", t, "/", waves)
    }

    # generate l_t for everyone (no censoring applied)
    mu_l <- rep(0, n)

    b_effects <- c(
      b1_t = p$b_l_coef * pmin(2, 1 + p$b_l_time_trend * t),
      b2_t = p$b_l_coef * pmax(0, 1 - p$b_l_time_decay * t),
      b3_t = p$b_l_coef * sin(pi * t / (waves * p$b_l_time_cycle))
    )

    if (validate_props) {
      l_coefs <- c(
        b_effects,
        a_lag = if (t > 1) p$a_l_coef else 0,
        y_lag = if (use_y_feedback && t > 1) p$y_l_coef else 0
      )
      l_coefs <- validate_proportions(l_coefs, paste0("t", t, "_l"))
      b_effects <- l_coefs[1:3]
      a_lag_coef <- l_coefs["a_lag"]
      y_lag_coef <- l_coefs["y_lag"]
    } else {
      a_lag_coef <- if (t > 1) p$a_l_coef else 0
      y_lag_coef <- if (use_y_feedback && t > 1) p$y_l_coef else 0
    }

    mu_l <- 0
    if (n_baselines >= 1) mu_l <- mu_l + b_effects[1] * b_mat[, 1]
    if (n_baselines >= 2) mu_l <- mu_l + b_effects[2] * b_mat[, 2]
    if (n_baselines >= 3) mu_l <- mu_l + b_effects[3] * b_mat[, 3]

    if (t > 1) {
      prev_a <- df[[paste0("t", t-1, "_a")]]
      mu_l <- mu_l + a_lag_coef * prev_a

      if (use_y_feedback && generate_y) {
        prev_y1 <- df[[paste0("t", t-1, "_y1")]]
        mu_l <- mu_l + y_lag_coef * prev_y1
      }
    }

    df[[paste0("t", t, "_l")]] <- mu_l + rnorm(n)

    # generate a_t for everyone
    mu_a <- rep(0, n)

    if (validate_props) {
      a_coefs <- c(
        b = p$b_a_coef,
        l = p$l_a_coef,
        a_lag = p$a_autoreg,
        y_lag = if (use_y_feedback) p$y_a_coef else 0
      )
      a_coefs <- validate_proportions(a_coefs, paste0("t", t, "_a"))
    }

    mu_a <- a_coefs["b"] * b_mat[, 1] +
      a_coefs["l"] * df[[paste0("t", t, "_l")]]

    prev_a <- if (t == 1) df$t0_a else df[[paste0("t", t-1, "_a")]]
    mu_a <- mu_a + a_coefs["a_lag"] * prev_a

    if (use_y_feedback && generate_y) {
      prev_y1 <- if (t == 1) df$t0_y1 else df[[paste0("t", t-1, "_y1")]]
      mu_a <- mu_a + a_coefs["y_lag"] * prev_y1
    }

    # generate natural treatment
    if (exposure_type == "binary") {
      natural_a <- rbern(n, expit(mu_a))
    } else {
      natural_a <- mu_a + rnorm(n)
    }

    # apply intervention if specified
    if (!is.null(intervention)) {
      temp_df <- df
      temp_df[[paste0("t", t, "_a")]] <- natural_a

      df[[paste0("t", t, "_a")]] <- intervention(
        temp_df,
        time = t,
        trt = paste0("t", t, "_a")
      )
    } else {
      df[[paste0("t", t, "_a")]] <- natural_a
    }

    # generate y_t if needed
    if (generate_y) {
      eps_y <- MASS::mvrnorm(n, rep(0, n_outcomes), sigma_y)
      if (n_outcomes == 1) eps_y <- matrix(eps_y, ncol = 1)

      for (j in seq_len(n_outcomes)) {
        mu_y <- rep(0, n)
        shrink <- if (j == 1) 1 else p[[paste0("y", j, "_shrink")]]

        prev_l <- if (t == 1) rep(0, n) else df[[paste0("t", t-1, "_l")]]
        prev_a <- if (t == 1) df$t0_a else df[[paste0("t", t-1, "_a")]]

        if (validate_props) {
          y_coefs <- c(
            b = p$b_y_coef,
            l_lag = p$l_y_coef,
            a_lag = shrink * p$a_lag_y_coef,
            y_lag = p$y_autoreg,
            cross = if (j > 1 && n_outcomes > 1) p[[paste0("y1_y", j, "_cross")]] else 0
          )
          y_coefs <- validate_proportions(y_coefs, paste0("t", t, "_y", j))
        }

        mu_y <- y_coefs["b"] * b_mat[, 1] +
          y_coefs["l_lag"] * prev_l +
          y_coefs["a_lag"] * prev_a

        mu_y <- mu_y +
          shrink * p$a_b_y_het * prev_a * b_mat[, 1] +
          shrink * p$a_l_y_het * prev_a * prev_l

        prev_y <- if (t == 1) df[[paste0("t0_y", j)]] else df[[paste0("t", t-1, "_y", j)]]
        mu_y <- mu_y + y_coefs["y_lag"] * prev_y

        if (j > 1 && n_outcomes > 1) {
          prev_y1 <- if (t == 1) df$t0_y1 else df[[paste0("t", t-1, "_y1")]]
          mu_y <- mu_y + y_coefs["cross"] * prev_y1
        }

        if (outcome_type == "continuous") {
          df[[paste0("t", t, "_y", j)]] <- mu_y + eps_y[, j]
        } else {
          df[[paste0("t", t, "_y", j)]] <- rbern(n, expit(mu_y + eps_y[, j]))
        }
      }
    }

    # calculate censoring probability for this wave (but don't apply)
    if (censoring$rate > 0) {
      cens_lp <- rep(qlogis(censoring$rate), n)

      prev_a <- if (t == 1) df$t0_a else df[[paste0("t", t-1, "_a")]]
      prev_l <- if (t == 1) rep(0, n) else df[[paste0("t", t-1, "_l")]]

      if (censoring$exposure_dependence) {
        cens_lp <- cens_lp + p$cens_a_coef * prev_a
      }

      if (isTRUE(censoring$l_dependence)) {
        cens_lp <- cens_lp + p$cens_l_coef * prev_l
      }

      if (isTRUE(censoring$y_dependence) && generate_y) {
        prev_y <- if (t == 1) df$t0_y1 else df[[paste0("t", t-1, "_y1")]]
        cens_lp <- cens_lp + p$cens_y_coef * prev_y
      }

      if (censoring$latent_dependence) {
        cens_lp <- cens_lp + p$cens_latent_sd * latent_frailty
      }

      censoring_probs[[t + 1]] <- expit(cens_lp)
    }
  }

  # final outcome (everyone gets this in complete data)
  if (generate_y) {
    t_final <- waves + 1

    eps_y <- MASS::mvrnorm(n, rep(0, n_outcomes), sigma_y)
    if (n_outcomes == 1) eps_y <- matrix(eps_y, ncol = 1)

    for (j in seq_len(n_outcomes)) {
      mu_y <- rep(0, n)
      shrink <- if (j == 1) 1 else p[[paste0("y", j, "_shrink")]]

      mu_y <- p$b_y_coef * b_mat[, 1] +
        p$l_y_coef * df[[paste0("t", waves, "_l")]] +
        shrink * p$a_lag_y_coef * df[[paste0("t", waves, "_a")]] +
        p$y_autoreg * df[[paste0("t", waves, "_y", j)]]

      mu_y <- mu_y +
        shrink * p$a_b_y_het * df[[paste0("t", waves, "_a")]] * b_mat[, 1] +
        shrink * p$a_l_y_het * df[[paste0("t", waves, "_a")]] * df[[paste0("t", waves, "_l")]]

      if (j > 1 && n_outcomes > 1) {
        mu_y <- mu_y + p[[paste0("y1_y", j, "_cross")]] * df[[paste0("t", waves, "_y1")]]
      }

      if (outcome_type == "continuous") {
        df[[paste0("t", t_final, "_y", j)]] <- mu_y + eps_y[, j]
      } else {
        df[[paste0("t", t_final, "_y", j)]] <- rbern(n, expit(mu_y + eps_y[, j]))
      }
    }
  }

  # clean up column names
  if (n_outcomes == 1 && generate_y) {
    y_cols <- grep("_y1$", names(df))
    names(df)[y_cols] <- gsub("_y1$", "_y", names(df)[y_cols])
  }

  # apply margot_process_longitudinal if requested
  if (apply_process_function) {
    # set up default process args
    default_process_args <- list(
      df_wide = df,
      exposure_vars = if (exposure_type == "binary" || exposure_type == "continuous") "a" else NULL,
      scale_continuous = FALSE,  # already handled
      not_lost_suffix = "not_lost_following_wave",
      preserve_temporal_order = TRUE
    )

    # merge with user-provided args
    process_args <- modifyList(default_process_args, process_args)

    # apply the function
    df <- do.call(margot_process_longitudinal, process_args)
  }

  # apply shadows if provided
  if (!is.null(shadows)) {
    # handle single shadow or list of shadows
    if (inherits(shadows, "margot_shadow")) {
      shadows <- list(shadows)
    } else if (!is.list(shadows)) {
      stop("shadows must be a shadow object or list of shadow objects")
    }
    
    df <- apply_shadows(df, shadows)
  }

  # add metadata
  attr(df, "margot_meta") <- list(
    params = p,
    y_feedback = y_feedback,
    n_outcomes = n_outcomes,
    n_baselines = n_baselines,
    exposure_type = exposure_type,
    outcome_type = outcome_type,
    structural_model = "semi-markovian",
    intervention_applied = !is.null(intervention),
    sampling_weights_applied = !is.null(sampling_weights),
    censoring_probs = censoring_probs,  # store for potential use
    waves = waves,
    n = n,
    format = "wide",
    timestamp = Sys.time(),
    shadows_applied = !is.null(shadows)
  )

  # handle long format conversion
  if (!wide) {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop("tidyr package is required for long format")
    }

    meta <- attr(df, "margot_meta")
    baseline_vars <- c("id", grep("^b[0-9]+$", names(df), value = TRUE))
    if ("sampling_weight" %in% names(df)) {
      baseline_vars <- c(baseline_vars, "sampling_weight")
    }

    long_df <- tidyr::pivot_longer(
      df,
      cols = -tidyr::all_of(baseline_vars),
      names_to = c("time", ".value"),
      names_pattern = "t([0-9]+)_(.*)"
    ) |>
      dplyr::mutate(time = as.integer(time)) |>
      dplyr::arrange(id, time)

    attr(long_df, "margot_meta") <- meta
    attr(long_df, "margot_meta")$format <- "long"

    return(long_df)
  }

  df
}

# ---- helper functions --------------------------------------------------
#' Default simulation parameter values
#' @keywords internal
.default_sim_params <- function() {
  list(
    # censoring parameters
    cens_rate       = 0.1,   # base censoring rate
    cens_a_coef     = 0.2,   # a_{k-1} ->> c_k
    cens_l_coef     = 0.1,   # l_{k-1} ->> c_k
    cens_y_coef     = 0.15,  # y_{k-1} ->> c_k
    cens_latent_sd  = 0.5,   # shared frailty sd

    # time-invariant baseline effects (b paths)
    b_l_coef        = 0.15,  # b ->> l (all times)
    b_a_coef        = 0.10,  # b ->> a (all times)
    b_y_coef        = 0.15,  # b ->> y (all times)

    # time-varying baseline effects on l
    b_l_time_trend  = 0.05,  # linear time trend for b1 ->> l
    b_l_time_decay  = 0.02,  # decay rate for b2 ->> l
    b_l_time_cycle  = 1.0,   # cycle length for b3 ->> l (as fraction of waves)

    # autoregressive effects
    a_autoreg       = 0.25,  # a_{t-1} ->> a_t
    y_autoreg       = 0.20,  # y_{t-1} ->> y_t

    # cross-lagged effects (only when y_feedback != "none")
    y_a_coef        = 0.15,  # y_{t-1} ->> a_t
    y_l_coef        = 0.10,  # y_{t-1} ->> l_t
    a_l_coef        = 0.10,  # a_{t-1} ->> l_t

    # contemporaneous effects
    l_a_coef        = 0.20,  # l_t ->> a_t

    # lagged effects on outcome
    l_y_coef        = 0.15,  # l_{t-1} ->> y_t
    a_lag_y_coef    = 0.25,  # a_{t-1} ->> y_t (main effect)

    # heterogeneous treatment effects
    a_b_y_het       = 0.10,  # a * b interaction ->> y
    a_l_y_het       = 0.05,  # a * l interaction ->> y

    # multiple outcome parameters
    y_cor           = 0.5,   # correlation between outcomes
    y2_shrink       = 0.8,   # shrinkage for y2 effects
    y3_shrink       = 0.6,   # shrinkage for y3 effects

    # cross-outcome effects (when n_outcomes > 1)
    y1_y2_cross     = 0.10,  # y1_{t-1} ->> y2_t
    y1_y3_cross     = 0.05,  # y1_{t-1} ->> y3_t
    y2_y3_cross     = 0.05   # y2_{t-1} ->> y3_t
  )
}

#' Validate that coefficients sum to less than 1
#' @keywords internal
validate_proportions <- function(coefs, node_name, threshold = 0.95) {
  total <- sum(abs(coefs), na.rm = TRUE)
  if (total >= threshold) {
    warning(sprintf(
      "coefficients for %s sum to %.2f (>= %.2f), reducing proportionally",
      node_name, total, threshold
    ))
    coefs <- coefs * (threshold - 0.05) / total
  }
  coefs
}