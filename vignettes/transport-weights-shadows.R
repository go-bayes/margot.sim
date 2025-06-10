## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
# Load package using devtools if not installed
if (!requireNamespace("margot.sim", quietly = TRUE)) {
  devtools::load_all()
} else {
  library(margot.sim)
}
library(ggplot2)
library(dplyr)

## ----basic-transport----------------------------------------------------------
# Generate data with transport weights
set.seed(2025)
data <- simulate_ate_data_with_weights(
  n_sample = 2000,
  n_population = 20000,
  p_z_sample = 0.1,      # 10% elderly in sample
  p_z_population = 0.5,   # 50% elderly in population
  beta_a = 1,            # base treatment effect
  beta_z = 0,            # being elderly doesn't affect baseline outcome
  beta_az = 2,           # treatment works 2 units better in elderly
  noise_sd = 1
)

# Look at the sample data
head(data$sample_data)

## ----compare-ates-------------------------------------------------------------
# Sample ATE (unweighted)
sample_ate <- with(data$sample_data, 
  mean(y_sample[a_sample == 1]) - mean(y_sample[a_sample == 0]))

# Population ATE (weighted)
pop_ate <- with(data$sample_data, {
  w1 <- weights[a_sample == 1]
  w0 <- weights[a_sample == 0]
  weighted.mean(y_sample[a_sample == 1], w1) - 
  weighted.mean(y_sample[a_sample == 0], w0)
})

# True population ATE from large population data
true_pop_ate <- with(data$population_data,
  mean(y_population[a_population == 1]) - mean(y_population[a_population == 0]))

cat("Sample ATE (unweighted):", round(sample_ate, 3), "\n")
cat("Population ATE (weighted):", round(pop_ate, 3), "\n")
cat("True Population ATE:", round(true_pop_ate, 3), "\n")
cat("Expected difference:", 2 * (0.5 - 0.1), "(due to effect modification)\n")

## ----weight-distribution------------------------------------------------------
# Examine weight values
weight_summary <- data$sample_data %>%
  group_by(z_sample) %>%
  summarise(
    n = n(),
    prop = n/nrow(data$sample_data),
    weight = first(weights)
  )

print(weight_summary)

# Visualize weight distribution
ggplot(data$sample_data, aes(x = factor(z_sample), y = weights)) +
  geom_boxplot() +
  geom_point(alpha = 0.1) +
  labs(x = "Effect Modifier Z", y = "Transport Weight",
       title = "Transport Weights by Effect Modifier Status") +
  theme_minimal()

## ----transport-with-shadows---------------------------------------------------
# Analysis with measurement error
result <- margot_transport_analysis(
  n_sample = 2000,
  p_z_sample = 0.1,
  p_z_population = 0.5,
  beta_a = 1,
  beta_z = 0,
  beta_az = 2,  # strong effect modification
  apply_shadows = TRUE,
  shadow_config = list(
    measurement_error = TRUE,
    missingness = FALSE
  ),
  seed = 2025
)

# Compare bias in sample vs population
print(result$bias_comparison)

## ----differential-missingness-------------------------------------------------
# Analysis with differential missingness
result2 <- margot_transport_analysis(
  n_sample = 2000,
  p_z_sample = 0.1,
  p_z_population = 0.5,
  beta_a = 1,
  beta_z = 0,
  beta_az = 2,
  apply_shadows = TRUE,
  shadow_config = list(
    measurement_error = FALSE,
    missingness = TRUE  # MAR depending on Z
  ),
  seed = 2025
)

print(result2$bias_comparison)

## ----multiple-shadows---------------------------------------------------------
# Both measurement error and missingness
result3 <- margot_transport_analysis(
  n_sample = 2000,
  p_z_sample = 0.1,
  p_z_population = 0.5,
  beta_a = 1,
  beta_z = 0,
  beta_az = 2,
  apply_shadows = TRUE,
  shadow_config = list(
    measurement_error = TRUE,
    missingness = TRUE
  ),
  seed = 2025
)

# Extract detailed results
cat("\n=== Sample Population (Unweighted) ===\n")
print(result3$comparison_sample$comparison)

cat("\n=== Target Population (Weighted) ===\n")
print(result3$comparison_population$comparison)

## ----comprehensive-comparison, fig.height=6-----------------------------------
# Run multiple scenarios
scenarios <- expand.grid(
  measurement_error = c(FALSE, TRUE),
  missingness = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Add a "clean" scenario
scenarios <- rbind(
  data.frame(measurement_error = FALSE, missingness = FALSE),
  scenarios[-1,]
)

# Run analyses
results_list <- list()
for (i in 1:nrow(scenarios)) {
  config <- list(
    measurement_error = scenarios$measurement_error[i],
    missingness = scenarios$missingness[i]
  )
  
  res <- margot_transport_analysis(
    n_sample = 1500,
    p_z_sample = 0.1,
    p_z_population = 0.5,
    beta_a = 1,
    beta_az = 2,
    apply_shadows = any(unlist(config)),
    shadow_config = config,
    seed = 2025 + i
  )
  
  if (any(unlist(config))) {
    results_list[[i]] <- res$bias_comparison %>%
      mutate(
        scenario = paste(
          ifelse(config$measurement_error, "ME", ""),
          ifelse(config$missingness, "Miss", ""),
          sep = "+"
        )
      )
  } else {
    # For clean data, create a comparison with no bias
    results_list[[i]] <- data.frame(
      Population = c("Sample (unweighted)", "Target (weighted)"),
      True_ATE = c(res$effects_sample$ate, res$effects_population$ate),
      Observed_ATE = c(res$effects_sample$ate, res$effects_population$ate),
      Bias = c(0, 0),
      Relative_Bias = c(0, 0),
      scenario = "Clean"
    )
  }
}

# Combine results
all_results <- do.call(rbind, results_list)

# Clean up scenario names
all_results$scenario <- trimws(gsub("\\+$", "", all_results$scenario))
all_results$scenario[all_results$scenario == ""] <- "Clean"

# Plot bias comparison
ggplot(all_results, aes(x = scenario, y = Bias, fill = Population)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Shadow Bias in Sample vs Target Population",
    subtitle = "Effect of measurement error (ME) and missingness (Miss) on transported estimates",
    x = "Shadow Scenario",
    y = "Bias in ATE Estimate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ----differential-bias-demo---------------------------------------------------
# Strong effect modification example
strong_em <- margot_transport_analysis(
  n_sample = 2000,
  p_z_sample = 0.2,
  p_z_population = 0.8,  # very different populations
  beta_a = 0.5,
  beta_az = 3,  # very strong effect modification
  apply_shadows = TRUE,
  shadow_config = list(measurement_error = TRUE, missingness = FALSE),
  seed = 42
)

# Weak effect modification example  
weak_em <- margot_transport_analysis(
  n_sample = 2000,
  p_z_sample = 0.2,
  p_z_population = 0.8,
  beta_a = 2,
  beta_az = 0.2,  # weak effect modification
  apply_shadows = TRUE,
  shadow_config = list(measurement_error = TRUE, missingness = FALSE),
  seed = 42
)

cat("Strong Effect Modification:\n")
print(strong_em$bias_comparison[, c("Population", "Bias", "Relative_Bias")])

cat("\nWeak Effect Modification:\n")
print(weak_em$bias_comparison[, c("Population", "Bias", "Relative_Bias")])

## ----weight-diagnostics-------------------------------------------------------
# Generate example with more extreme weight scenario
extreme_data <- simulate_ate_data_with_weights(
  n_sample = 1000,
  p_z_sample = 0.05,   # very few elderly in sample
  p_z_population = 0.7  # mostly elderly in population
)

# Weight diagnostics
weights <- extreme_data$sample_data$weights
cat("Weight Summary:\n")
cat("  Mean:", mean(weights), "\n")
cat("  SD:", sd(weights), "\n")
cat("  Min:", min(weights), "\n")
cat("  Max:", max(weights), "\n")
cat("  Effective Sample Size:", sum(weights)^2 / sum(weights^2), "\n")

# Visualize weight distribution
hist(weights, breaks = 30, main = "Distribution of Transport Weights",
     xlab = "Weight", col = "lightblue")
abline(v = 1, col = "red", lty = 2, lwd = 2)

