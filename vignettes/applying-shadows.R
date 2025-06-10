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

