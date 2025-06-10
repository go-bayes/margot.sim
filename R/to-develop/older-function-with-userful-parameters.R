# ---------------- margot_simulate.R (self‑contained) -------------------------
# apply_censoring()  – monotone attrition helper with optional jitter
# margot_simulate()  – longitudinal data generator
# -----------------------------------------------------------------------------

#' Apply monotone attrition censoring (with optional jitter)
#'
#' Wrap a long‐format data set in which rows are uniquely identified
#' by `id` and `wave`, dropping (masking) values **after** the first
#' observed censoring indicator equals 0.  By default the helper
#' censors *all* subsequent variables, but you may add a small random
#' offset (positive or negative) via `jitter = TRUE` to avoid inducing
#' identical missing‐data patterns across columns.
#'
#' @param data        a `data.frame` / `tibble` with columns
#'   `id`, `wave`, and the binary censoring variable passed in
#'   `censor_var`.
#' @param censor_var  the name of the censoring indicator
#'   (1 = observed, 0 = censored). default `"C"`.
#' @param jitter      logical. if `TRUE`, each target variable is
#'   censored from a wave randomly shifted by `jitter_range`.
#' @param jitter_range integer vector giving the possible wave offsets
#'   when `jitter = TRUE`. default `-1:1`.
#' @param seed        optional integer to reproduce the jitter draw.
#'
#' @return an object of the same class as `data`, with post-censoring
#'   values replaced by `NA`.  all original rows are preserved.
#'
#' @examples
#' # build a toy panel with deterministic censoring
#' library(dplyr)
#' dat <- tidyr::expand_grid(id = 1:3, wave = 1:5) %>%
#'   mutate(
#'     C   = as.integer(wave < c(3, 4, 2)[id]),   # first missing at waves 3,4,2
#'     y   = rnorm(n()),
#'     z   = rnorm(n())
#'   )
#'
#' # (i) strict monotone censoring
#' apply_censoring(dat)
#'
#' # (ii) allow column-specific jitter so y and z drop out at different times
#' apply_censoring(dat, jitter = TRUE, seed = 99)
#'
#' @import dplyr
#' @seealso \code{\link{margot_simulate}}
#' @export
apply_censoring <- function(data,
                            censor_var   = "C",
                            jitter       = FALSE,
                            jitter_range = -1:1,
                            seed         = NULL) {
  stopifnot(all(c("id", "wave", censor_var) %in% names(data)))
  if (!is.null(seed)) set.seed(seed)

  data <- data |>
    dplyr::group_by(id) |>
    dplyr::mutate(first_censor = {
      w <- wave[.data[[censor_var]] == 0]
      if (length(w)) min(w) else Inf
    }) |>
    dplyr::ungroup()

  skip_cols <- c("id", "wave", censor_var, "first_censor",
                 grep("_not_censored$", names(data), value = TRUE))
  targets   <- setdiff(names(data), skip_cols)
  offsets   <- if (jitter) setNames(sample(jitter_range, length(targets), TRUE), targets)
  else        setNames(rep(0L, length(targets)), targets)

  for (v in targets) {
    thr <- pmax(data$first_censor + offsets[[v]], 1)
    data[[v]] <- ifelse(data$wave >= thr, NA, data[[v]])
  }

  data |>
    dplyr::select(-first_censor)
}

# ---- 2. simulator ----------------------------------------------------------
#' Simulate longitudinal exposures, outcomes, and covariates
#'
#' `margot_simulate()` draws baseline covariates, time-varying covariates
#' (`L`), exposures (`A`), and lead outcomes (`Y`) for a synthetic panel
#' study. Monotone attrition is introduced at two points: immediately before
#' measuring time-varying covariates and again (optionally) just before
#' outcomes are recorded. The user controls feedback strength from past
#' exposure to future covariates/outcomes, exposure–outcome relations,
#' positivity, and measurement error.
#'
#' @section heterogeneous effects:
#' supply an element `het` inside an exposure spec to generate exposure ×
#' baseline-modifier interactions. the simulator adds
#' \(\gamma_k A \times B_k\) to the outcome mean (or to the logit for binary
#' outcomes) for every pair of `modifier[k]` and `coef[k]`.
#'
#' @param n                   number of individuals.
#' @param waves               number of follow-up waves (exposures and
#'   time-varying covariates). outcomes are returned for wave `waves + 1`.
#' @param exposures           named list describing each exposure. see
#'   *details*.
#' @param outcomes            named list describing outcomes. default is a
#'   single continuous outcome called `"Y"`.
#' @param p_covars            number of baseline (`B`) covariates.
#' @param censoring           list of attrition control parameters; at minimum
#'   `rate`. set `exposure_dependence = TRUE` to make attrition depend on the
#'   previous exposure, and/or `latent_dependence = TRUE` to add shared
#'   frailty.
#' @param item_missing_rate   marginal probability an observed value is
#'   replaced by `NA` (mcar).
#' @param exposure_outcome    coefficient for the exposure ->> outcome path.
#' @param covar_feedback      strength of lagged covariate feedback on
#'   exposure assignment.
#' @param y_feedback          optional feedback from the most recent
#'   outcome to current exposure (when `exposures[[1]]$lag_Y = TRUE`).
#' @param jitter_censoring    passed to `apply_censoring()` for final block.
#' @param positivity          `"good"` (balanced), `"poor"` (skewed
#'   assignment), or a numeric probability in (0,1).
#' @param outcome_type        shortcut for a single outcome: `"continuous"`
#'   (default) or `"binary"`.
#' @param wide                logical; if `FALSE` a long-format data set is
#'   returned.
#' @param seed                integer for reproducibility.
#' @param ...                 absorbed for future arguments.
#'
#' @return
#' a `tibble` in wide or long form containing baseline `B` variables,
#' `L` covariates, `A` exposures, and lead outcomes `Y`. attributes include
#' the matched call and a timestamp.
#'
#' @examples
#' ## ---- basic usage -------------------------------------------------------
#' sim1 <- margot_simulate(n = 100, waves = 3, seed = 1)
#' dplyr::glimpse(sim1)
#'
#' ## ---- heterogeneous treatment effect: single baseline modifier ---------
#' dat_het <- margot_simulate(
#'   n        = 5000,
#'   waves    = 3,
#'   p_covars = 3,       # B1, B2, B3
#'   exposures = list(
#'     A1 = list(
#'       type = "binary",
#'       het  = list(modifier = "B2",  # effect modifier
#'                   coef     = 0.6)   # γ strength
#'     )
#'   ),
#'   outcomes  = list(
#'     Y = list(type = "normal")
#'   ),
#'   censoring = list(rate = 0.15, exposure_dependence = TRUE),
#'   item_missing_rate = 0.02,
#'   exposure_outcome  = 0.8,          # main effect β of A1
#'   seed = 2026
#' )
#'
#' # quick check: linear model for observed Y
#' fit <- lm(t4_Y ~ t3_A1 * B2, data = dat_het)
#' summary(fit)$coefficients
#'
#' # visualise interaction
#' library(ggplot2)
#' ggplot(dat_het, aes(B2, t4_Y, colour = factor(t3_A1))) +
#'   geom_smooth(method = "lm", se = FALSE) +
#'   labs(colour = "A1 at wave 3",
#'        y = "Outcome Y (wave 4)",
#'        title = "effect of A1 varies with baseline B2") +
#'   theme_minimal()
#'
#' ## ---- heterogeneous effect: two modifiers ------------------------------
#' dat_het2 <- margot_simulate(
#'   n        = 3000,
#'   waves    = 2,
#'   p_covars = 3,
#'   exposures = list(
#'     A1 = list(
#'       type = "binary",
#'       het  = list(
#'         modifier = c("B2", "B3"),
#'         coef     = c(0.6, -0.4)     # γ₁·A1·B2 + γ₂·A1·B3
#'       )
#'     )
#'   ),
#'   seed = 123
#' )
#'
#' @import dplyr tibble MASS ggplot2
#' @importFrom stats rnorm rbinom qlogis plogis pnorm lm
#' @seealso \code{\link{apply_censoring}}
#' @export
margot_simulate <- function(
    n,
    waves,
    exposures          = NULL,
    outcomes           = NULL,
    p_covars           = 20,
    censoring          = list(rate = 0.25),
    item_missing_rate  = 0,
    exposure_outcome   = 0.5,
    covar_feedback     = 0.2,
    y_feedback         = 0.4,
    jitter_censoring   = FALSE,
    positivity         = "good",
    outcome_type       = NULL,
    wide               = TRUE,
    seed               = NULL,
    ...) {

  if (!is.null(seed)) set.seed(seed)

  # fill missing censoring parameters ---------------------------------------
  censoring <- modifyList(list(rate = 0.25,
                               exposure_dependence = FALSE,
                               latent_dependence   = FALSE,
                               latent_rho          = 0.5), censoring)

  # default exposure and outcome specs --------------------------------------
  if (is.null(exposures)) exposures <- list(A1 = list(type = "binary"))
  if (is.null(outcomes)) {
    if (is.null(outcome_type) || outcome_type[1] == "continuous") {
      outcomes <- list(Y = list(type = "normal"))
    } else if (outcome_type[1] == "binary") {
      outcomes <- list(Y = list(type = "binary", p = 0.5))
    } else stop("Unknown outcome_type")
  }
  k_out <- length(outcomes)

  # helper fns --------------------------------------------------------------
  rbern <- function(x, p = NULL) {
    prob <- if (is.null(p)) x else if (length(p) == 1) rep(p, x) else p
    prob[is.na(prob) | prob < 0] <- 0; prob[prob > 1] <- 1
    stats::rbinom(length(prob), 1, prob)
  }
  sprinkle <- function(v) {
    if (item_missing_rate == 0) return(v)
    v[runif(length(v)) < item_missing_rate] <- NA
    v
  }

  # baseline positivity -----------------------------------------------------
  pos_prob <- switch(as.character(positivity[1]),
                     good = 0.5,
                     poor = 0.9,
                     as.numeric(positivity[1]))
  if (is.na(pos_prob) || pos_prob <= 0 || pos_prob >= 1) pos_prob <- 0.5

  # baseline covariates and exposures --------------------------------------
  Sigma_B <- matrix(0.3, p_covars, p_covars); diag(Sigma_B) <- 1
  df <- tibble::tibble(id = seq_len(n)) |>
    dplyr::bind_cols(
      tibble::as_tibble(MASS::mvrnorm(n, rep(0, p_covars), Sigma_B),
                        .name_repair = ~ paste0("B", seq_along(.x)))
    )
  for (ex in names(exposures)) {
    df[[paste0("t0_", ex)]] <- if (exposures[[ex]]$type == "binary") rbern(n, pos_prob) else rnorm(n)
  }

  # trackers ----------------------------------------------------------------
  alive     <- rep(TRUE, n)
  last_Y    <- matrix(0, n, k_out)
  long_list <- if (!wide) vector("list", waves) else NULL

  for (t in seq_len(waves)) {
    tag      <- paste0("t", t)
    prev_tag <- if (t == 1) "t0" else paste0("t", t - 1)

    # wave-level attrition ------------------------------------------------
    lp <- rep(-Inf, n)
    idx_alive <- which(alive)
    if (length(idx_alive)) {
      lp[idx_alive] <- qlogis(1 - censoring$rate)
      if (isTRUE(censoring$exposure_dependence)) {
        prev_A <- df[[paste0(prev_tag, "_", names(exposures)[1])]][idx_alive]
        lp[idx_alive] <- lp[idx_alive] + 0.4 * prev_A
      }
      if (isTRUE(censoring$latent_dependence)) {
        lp[idx_alive] <- lp[idx_alive] + censoring$latent_rho * rnorm(length(idx_alive))
      }
    }
    C_wave <- rbern(plogis(lp))
    alive  <- alive & as.logical(C_wave)

    # L variables --------------------------------------------------------
    L_df <- tibble::tibble(.rows = n)
    idx  <- which(alive)
    for (j in 1:3) {
      v <- rep(NA_real_, n); v[idx] <- rnorm(length(idx))
      L_df[[paste0(tag, "_L", j)]] <- sprinkle(v)
    }

    # exposures ---------------------------------------------------------
    A_df <- tibble::tibble(.rows = n); A_cols <- list()
    for (ex in names(exposures)) {
      spec <- exposures[[ex]]
      if (isTRUE(spec$lag_Y) && is.null(spec$lag_coef)) spec$lag_coef <- y_feedback
      v <- rep(NA_real_, n)
      if (length(idx)) {
        if (spec$type == "binary") {
          lpA <- -0.2 + covar_feedback * L_df[[paste0(tag, "_L1")]][idx]
          if (isTRUE(spec$lag_Y)) lpA <- lpA + spec$lag_coef * last_Y[idx, 1]
          v[idx] <- rbern(plogis(lpA))
        } else {
          v[idx] <- rnorm(length(idx), mean = covar_feedback * L_df[[paste0(tag, "_L1")]][idx])
        }
      }
      A_cols[[ex]] <- v
      A_df[[paste0(tag, "_", ex)]] <- sprinkle(v)
    }

    # lead‑1 attrition before outcomes ----------------------------------
    Y_df <- tibble::tibble(.rows = n)
    if (t == waves) {
      lpYattr <- rep(-Inf, n)
      idx_alive2 <- which(alive)
      if (length(idx_alive2)) {
        lpYattr[idx_alive2] <- qlogis(1 - censoring$rate)
        if (isTRUE(censoring$exposure_dependence)) lpYattr[idx_alive2] <- lpYattr[idx_alive2] + 0.4 * A_cols[[1]][idx_alive2]
        if (isTRUE(censoring$latent_dependence))   lpYattr[idx_alive2] <- lpYattr[idx_alive2] + censoring$latent_rho * rnorm(length(idx_alive2))
      }
      keep_Y <- rbern(plogis(lpYattr))
      alive_Y <- alive & as.logical(keep_Y)

      idx_Y <- which(alive_Y)
      Err <- if (length(idx_Y)) MASS::mvrnorm(length(idx_Y), rep(0, k_out), diag(k_out)) else matrix(NA_real_, 0, k_out)
      Err <- matrix(Err, nrow = length(idx_Y), ncol = k_out)
      for (k in seq_along(outcomes)) {
        spec <- outcomes[[k]]; y <- rep(NA_real_, n)
        if (length(idx_Y)) {
          mu <- exposure_outcome * A_cols[[1]][idx_Y] + 0.1 * df$B1[idx_Y]
          if (!is.null(exposures[[1]]$het)) {
            gamma  <- exposures[[1]]$het$coef
            modcol <- exposures[[1]]$het$modifier
            mu <- mu + gamma * A_cols[[1]][idx_Y] * df[[modcol]][idx_Y]
          }
          if (spec$type == "normal") {
            y[idx_Y] <- mu + Err[, k]
          } else {
            lp <- -1 + mu
            y[idx_Y] <- rbern(pnorm(lp + Err[, k]))
          }
        }
        Y_df[[paste0("t", t + 1, "_", names(outcomes)[k])]] <- sprinkle(y)
        last_Y[, k] <- y
      } # end outcome loop
    } # end final wave block

    # bind wave data ----------------------------------------------------
    df <- dplyr::bind_cols(df, L_df, A_df, Y_df)
    if (!wide) long_list[[t]] <- dplyr::bind_cols(id = df$id, wave = t, L_df, A_df, Y_df)
  } # end wave loop

  # baseline MCAR sprinkling -------------------------------------------
  if (item_missing_rate > 0) {
    bcols <- grep("^B", names(df), value = TRUE)
    df[bcols] <- lapply(df[bcols], sprinkle)
  }

  # drop internal C column -------------------------------------------------
  if ("C" %in% names(df)) df <- df[, !names(df) %in% "C"]

  # attach metadata ----------------------------------------------------
  attr(df, "margot_meta") <- list(args = match.call(), timestamp = Sys.time())

  if (wide) return(df)

  # assemble long ------------------------------------------------------
  base_block <- df |>
    dplyr::select(id, dplyr::starts_with("t0_"), dplyr::starts_with("B")) |>
    dplyr::mutate(wave = 0) |>
    dplyr::relocate(wave, .after = id)

  long_out <- dplyr::bind_rows(long_list)
  attr(long_out, "margot_meta") <- attr(df, "margot_meta")

  dplyr::bind_rows(base_block, long_out)
}
