## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(margot.sim)

## ----basic-sim----------------------------------------------------------------
# Simulate basic longitudinal data
set.seed(123)
sim_data <- margot_simulate(
  n = 1000,      # Number of subjects
  waves = 3,     # Number of measurement waves
  n_outcomes = 1 # Number of outcomes (default)
)

# View structure
str(sim_data[1:5, 1:10])

## ----custom-sim---------------------------------------------------------------
# Custom simulation with specific parameters
sim_custom <- margot_simulate(
  n = 500,
  waves = 4,
  n_baselines = 3,        # Fewer baseline covariates
  exposure_type = "continuous",  # Continuous exposure
  outcome_type = "binary",       # Binary outcome
  params = list(
    b_a_coef = 0.3,      # Baseline → exposure effect
    a_y_coef = 0.5,      # Exposure → outcome effect
    b_y_coef = 0.2       # Baseline → outcome effect
  )
)

# Check exposure and outcome types
summary(sim_custom$t1_a)  # Continuous
table(sim_custom$t2_y)    # Binary

## ----censoring----------------------------------------------------------------
# Simulate with censoring
sim_censored <- margot_simulate(
  n = 1000,
  waves = 3,
  censoring = list(
    rate = 0.2,                    # 20% censoring rate per wave
    exposure_dependence = TRUE,    # Censoring depends on exposure
    y_dependence = TRUE           # Censoring depends on outcome
  )
)

# Check censoring indicators
table(sim_censored$t0_not_lost_following_wave)
table(sim_censored$t1_not_lost_following_wave)

## ----long-format--------------------------------------------------------------
# Generate in long format
sim_long <- margot_simulate(
  n = 200,
  waves = 3,
  wide = FALSE  # Request long format
)

# View structure
head(sim_long)

# Number of observations per person
table(table(sim_long$id))

## ----interventions------------------------------------------------------------
# Define an intervention that sets treatment to 1 for everyone
always_treat <- function(data, time, trt) {
  rep(1, nrow(data))
}

# Simulate under intervention
sim_intervention <- margot_simulate(
  n = 500,
  waves = 2,
  intervention = always_treat
)

# Verify intervention was applied
table(sim_intervention$t1_a)  # All 1s

