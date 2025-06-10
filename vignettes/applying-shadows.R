## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(margot.sim)
library(ggplot2)

## ----basic-shadow-------------------------------------------------------------
# Generate clean data
set.seed(456)
clean_data <- margot_simulate(
  n = 1000,
  waves = 2,
  apply_process_function = FALSE  # Keep raw data
)

# Create a measurement error shadow
me_shadow <- create_shadow(
  type = "measurement_error",
  params = list(
    error_type = "classical",
    variables = c("t1_a", "t2_a"),  # Apply to exposures
    sigma = 0.3  # Standard deviation of error
  )
)

# Apply the shadow
data_with_error <- apply_shadow(clean_data, me_shadow)

# Compare clean vs error-prone measurements
plot(clean_data$t1_a, data_with_error$t1_a,
     xlab = "True Exposure", ylab = "Measured Exposure",
     main = "Classical Measurement Error")
abline(0, 1, col = "red", lty = 2)

## ----missingness--------------------------------------------------------------
# Missing Completely at Random (MCAR)
mcar_shadow <- create_item_missingness_shadow(
  variables = c("t1_y", "t2_y"),
  missing_rate = 0.2,
  missing_mechanism = "MCAR"
)

# Missing at Random (MAR) - depends on observed variables
mar_shadow <- create_item_missingness_shadow(
  variables = c("t1_y", "t2_y"),
  missing_rate = 0.3,
  missing_mechanism = "MAR",
  dependent_vars = c("b1", "t0_a")  # Missingness depends on these
)

# Apply both shadows
data_mcar <- apply_shadow(clean_data, mcar_shadow)
data_mar <- apply_shadow(clean_data, mar_shadow)

# Compare missingness rates
cat("MCAR missingness rate:", mean(is.na(data_mcar$t1_y)), "\n")
cat("MAR missingness rate:", mean(is.na(data_mar$t1_y)), "\n")

## ----multiple-shadows---------------------------------------------------------
# Create multiple shadows
shadow1 <- create_shadow(
  type = "measurement_error",
  params = list(
    error_type = "classical",
    variables = "t1_a",
    sigma = 0.2
  )
)

shadow2 <- create_item_missingness_shadow(
  variables = c("t2_y"),
  missing_rate = 0.25,
  missing_mechanism = "MAR",
  dependent_vars = c("t1_a", "t1_y")
)

# Apply shadows sequentially
data_shadowed <- clean_data |>
  apply_shadow(shadow1) |>
  apply_shadow(shadow2)

# Or use apply_shadows for multiple at once
shadows <- list(shadow1, shadow2)
data_shadowed2 <- apply_shadows(clean_data, shadows)

# Check results
sum(is.na(data_shadowed2$t2_y))

## ----positivity---------------------------------------------------------------
# Create positivity shadow that removes extreme covariate values
# Filter function keeps only observations where b1 and b2 are in reasonable range
pos_shadow <- create_positivity_shadow(
  exposure_var = "t1_a",
  filter_fn = function(data) {
    # Keep observations where covariates are within central 90%
    b1_limits <- quantile(data$b1, c(0.05, 0.95))
    b2_limits <- quantile(data$b2, c(0.05, 0.95))
    data$b1 >= b1_limits[1] & data$b1 <= b1_limits[2] &
    data$b2 >= b2_limits[1] & data$b2 <= b2_limits[2]
  }
)

data_trimmed <- apply_shadow(clean_data, pos_shadow)

# Compare sample sizes
cat("Original n:", nrow(clean_data), "\n")
cat("After positivity trimming:", nrow(data_trimmed), "\n")

## ----analyze-shadows----------------------------------------------------------
# Analyze the effect of measurement error
shadow_effects <- analyse_shadow_effects(
  original = clean_data,
  shadowed = data_with_error,
  variables = c("t1_a", "t2_a", "t2_y")
)

print(shadow_effects)

## ----selection-example--------------------------------------------------------
# Generate population data
population <- margot_simulate(
  n = 5000,
  waves = 2,
  params = list(
    b_a_coef = 0.4,
    a_y_coef = 0.6
  )
)

# Create selection shadow - higher SES more likely to participate
selection_shadow <- create_shadow(
  type = "selection",
  params = list(
    selection_type = "custom",
    selection_prob_fn = function(data) {
      # Higher b1 and b2 values increase selection probability
      # Normalize to 0-1 scale
      ses_score <- (data$b1 + data$b2) / 2
      ses_normalized <- (ses_score - min(ses_score)) / (max(ses_score) - min(ses_score))
      # Map to 30-70% selection probability range
      0.3 + 0.4 * ses_normalized
    }
  )
)

# Apply selection
study_sample <- apply_shadow(population, selection_shadow)

# Compare distributions
par(mfrow = c(1, 2))
hist(population$b1, main = "Population SES", xlab = "SES Score")
hist(study_sample$b1, main = "Study Sample SES", xlab = "SES Score")

## ----complete-example---------------------------------------------------------
# Start with clean data
set.seed(789)
true_data <- margot_simulate(
  n = 2000,
  waves = 3,
  n_outcomes = 2
)

# Create realistic shadows
shadows <- list(
  # Measurement error in exposure
  create_shadow(
    type = "measurement_error",
    params = list(
      error_type = "classical",
      variables = c("t1_a", "t2_a"),
      sigma = 0.25
    )
  ),
  
  # Differential missingness in outcomes
  create_item_missingness_shadow(
    variables = c("t2_y1", "t2_y2", "t3_y1", "t3_y2"),
    missing_rate = 0.2,
    missing_mechanism = "MAR",
    dependent_vars = c("t1_a", "b1")
  ),
  
  # Selection on baseline characteristics
  create_positivity_shadow(
    exposure_var = "t1_a",
    filter_fn = function(data) {
      # Keep central 80% based on covariate values
      # This ensures overlap in covariate distributions across treatment levels
      cov_score <- rowMeans(scale(data[, c("b1", "b2", "b3")]))
      cov_limits <- quantile(cov_score, c(0.1, 0.9))
      cov_score >= cov_limits[1] & cov_score <= cov_limits[2]
    }
  )
)

# Apply all shadows
observed_data <- apply_shadows(true_data, shadows)

# Summary of data quality
cat("Original sample size:", nrow(true_data), "\n")
cat("After shadows:", nrow(observed_data), "\n")
cat("Outcome missingness rate:", 
    mean(is.na(observed_data[, grep("y", names(observed_data))])), "\n")

