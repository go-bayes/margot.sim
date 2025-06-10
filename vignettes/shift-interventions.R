## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(margot.sim)

## ----lmtp-style---------------------------------------------------------------
# Define score bounds
min_score <- 1
max_score <- 7

# Create shift functions matching lmtp style
shift_up <- function(data, time, trt) {
  # Keep baseline (t0) as observed
  if (time == 0) {
    return(data[[trt]])
  }
  
  # Apply shift at wave 1
  if (time == 1) {
    values <- data[[trt]]
    return(ifelse(values <= max_score - 1, values + 1, max_score))
  }
  
  # No treatment at later waves
  return(rep(0, nrow(data)))
}

shift_down <- function(data, time, trt) {
  # Keep baseline (t0) as observed  
  if (time == 0) {
    return(data[[trt]])
  }
  
  # Apply shift at wave 1
  if (time == 1) {
    values <- data[[trt]]
    return(ifelse(values >= min_score + 1, values - 1, min_score))
  }
  
  # No treatment at later waves
  return(rep(0, nrow(data)))
}

# Natural (observed) intervention for comparison
natural <- function(data, time, trt) {
  data[[trt]]
}

## ----simulation-example-------------------------------------------------------
# Simulate data under different shift interventions
results <- margot_simulate_causal(
  n = 1000,
  waves = 3,
  treatments = "a",
  interventions = list(
    natural = natural,
    shift_up = shift_up,
    shift_down = shift_down
  ),
  common_params = list(
    params = list(
      a_lag_y_coef = 0.3,  # True causal effect
      exposure_type = "continuous"
    )
  ),
  seed = 2025
)

# Compare effects
print(results)

## ----helper-functions---------------------------------------------------------
# Create bounded shift interventions
shifts <- create_lmtp_style_shifts(
  min_score = 1,
  max_score = 7,
  baseline_wave = 0,  # Keep baseline as observed
  shift_wave = 1      # Apply shift at wave 1
)

# Use in simulation
sim_data <- margot_simulate(
  n = 500,
  waves = 2,
  exposure_type = "continuous",
  intervention = shifts$shift_up,
  seed = 123
)

# Check the intervention worked
cat("Mean exposure at baseline:", mean(sim_data$t0_a), "\n")
cat("Mean exposure at wave 1:", mean(sim_data$t1_a), "\n")

## ----threshold-shifts---------------------------------------------------------
# Shift everyone below score 3 up to 3
threshold_shift <- create_threshold_shift(
  threshold = 3,
  shift_to = 3,
  direction = "up",
  start_wave = 1
)

# Simulate
sim_threshold <- margot_simulate(
  n = 500,
  waves = 2,
  exposure_type = "continuous",
  intervention = threshold_shift,
  seed = 456
)

# Verify no one below 3 at wave 1
cat("Proportion below 3 at wave 1:", 
    mean(sim_threshold$t1_a < 3, na.rm = TRUE), "\n")

## ----wave-specific------------------------------------------------------------
# Different interventions at different waves
wave_interventions <- create_wave_specific_shift(
  wave_shifts = list(
    "0" = function(x) x,                    # No change at baseline
    "1" = function(x) pmin(x + 1, 7),       # Shift up by 1
    "2" = function(x) pmax(x - 0.5, 1)      # Shift down by 0.5
  )
)

# Simulate
sim_waves <- margot_simulate(
  n = 500,
  waves = 2,
  exposure_type = "continuous",
  intervention = wave_interventions,
  seed = 789
)

## ----single-point-------------------------------------------------------------
# Intervention only at wave 1, natural elsewhere
single_point_shift <- function(data, time, trt) {
  if (time == 1) {
    # Apply your shift logic here
    values <- data[[trt]]
    return(pmin(values + 1, max_score))
  } else {
    # Natural values at all other times
    return(data[[trt]])
  }
}

## ----binary-shifts------------------------------------------------------------
# Increase probability of treatment by 20%
prob_shift <- function(data, time, trt) {
  if (time == 0) return(data[[trt]])
  
  # Get current treatment probability
  current_prob <- mean(data[[trt]])
  
  # Increase by 20% (bounded by 1)
  new_prob <- min(current_prob * 1.2, 1)
  
  # Resample with new probability
  rbinom(nrow(data), 1, new_prob)
}

## ----covariate-dependent------------------------------------------------------
# Shift based on confounder value
smart_shift <- function(data, time, trt) {
  if (time == 0) return(data[[trt]])
  
  # Get time-varying confounder
  l_var <- paste0("t", time, "_l")
  
  if (l_var %in% names(data)) {
    # Shift more for high-risk individuals (L > 0)
    values <- data[[trt]]
    high_risk <- data[[l_var]] > 0
    
    # Larger shift for high-risk
    values[high_risk] <- pmin(values[high_risk] + 2, max_score)
    values[!high_risk] <- pmin(values[!high_risk] + 1, max_score)
    
    return(values)
  } else {
    return(data[[trt]])
  }
}

## ----comparison---------------------------------------------------------------
# Run simulation comparing interventions
comparison_results <- margot_simulate_causal(
  n = 1000,
  waves = 3,
  treatments = "a",
  interventions = list(
    natural = function(data, time, trt) data[[trt]],
    shift_up_1 = create_shift_intervention(1, 1, 7, start_wave = 1),
    shift_up_2 = create_shift_intervention(2, 1, 7, start_wave = 1),
    threshold_3 = create_threshold_shift(3, 3, "up", start_wave = 1)
  ),
  common_params = list(
    params = list(a_lag_y_coef = 0.3)
  ),
  seed = 999
)

# Extract and compare effects
effects <- comparison_results$effects$estimate
names(effects) <- comparison_results$effects$contrast

print(effects)

