
<!-- README.md is generated from README.Rmd. Please edit that file -->

# margot.sim

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Codecov test
coverage](https://codecov.io/gh/go-bayes/margot.sim/graph/badge.svg)](https://app.codecov.io/gh/go-bayes/margot.sim)
[![GitHub
stars](https://img.shields.io/github/stars/go-bayes/margot.sim?style=social)](https://github.com/go-bayes/margot.sim)
<!-- CRAN badges: will activate after CRAN release
[![CRAN status](https://www.r-pkg.org/badges/version/margot.sim)](https://CRAN.R-project.org/package=margot.sim)
[![Downloads](https://cranlogs.r-pkg.org/badges/margot.sim)](https://cran.r-project.org/package=margot.sim)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/margot.sim)](https://cran.r-project.org/package=margot.sim)
--> <!-- badges: end -->

R package for simulating longitudinal data with realistic observational
shadows (measurement error, missingness, selection bias) and evaluating
causal inference methods via Monte Carlo simulation.

## Installation

You can install the development version of margot.sim from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("go-bayes/margot.sim")
```

## Overview

margot.sim extends the `margot` package with:

1.  **Shadowing Framework** - Apply observational distortions
    (measurement error, missingness, selection bias)
2.  **Monte Carlo Framework** - Systematically evaluate statistical
    estimators  
3.  **Flexible Distributions** - Specify non-normal distributions
4.  **Integrated Workflows** - Complete simulation studies

## Quick Start

``` r
library(margot.sim)

# Generate data with measurement error
shadow <- create_shadow(
  type = "measurement_error",
  params = list(
    variables = c("t1_l", "t2_l"),
    error_type = "classical",
    sigma = 0.5
  )
)

dat <- margot_simulate(
  n = 1000,
  waves = 3,
  shadows = shadow
)

# Run Monte Carlo evaluation
mc_results <- margot_monte_carlo(
  n_reps = 100,
  n_per_rep = 500,
  dgp_params = list(waves = 2),
  shadows = list(shadow),
  estimator_fn = function(data) {
    fit <- lm(t3_y ~ t2_a + t1_l + b1, data = data)
    list(
      estimate = coef(fit)["t2_a"],
      se = sqrt(diag(vcov(fit)))["t2_a"]
    )
  }
)

print(mc_results)
```

## Shadowing Framework

### Available Shadow Types

**Measurement Error:** - Classical error (continuous variables) -
Misclassification (binary variables) - Differential error -
Dichotomization - Correlated errors

``` r
# Classical measurement error
me_shadow <- create_shadow(
  type = "measurement_error",
  params = list(
    variables = "t1_l",
    error_type = "classical",
    sigma = 0.5
  )
)

# Misclassification for binary variables
misclass_shadow <- create_shadow(
  type = "measurement_error", 
  params = list(
    variables = "t1_a",
    error_type = "misclassification",
    sensitivity = 0.85,  # P(observed=1|true=1)
    specificity = 0.90   # P(observed=0|true=0)
  )
)
```

**Missing Data:**

``` r
# Item-level missingness
miss_shadow <- create_item_missingness_shadow(
  variables = c("t1_l", "t2_l"),
  missing_rate = 0.2,
  missing_mechanism = "MAR",
  dependent_vars = "b1"
)
```

**Selection Bias:**

``` r
# Positivity violations
pos_shadow <- create_positivity_shadow(
  exposure_var = "t1_a",
  filter_fn = function(data) {
    # Treatment only possible if risk score <= 2
    data$b1 + data$b2 <= 2
  }
)
```

### Combining Shadows

``` r
# Apply multiple shadows
shadows <- list(me_shadow, miss_shadow)
dat <- margot_simulate(n = 1000, waves = 3, shadows = shadows)

# Or apply post-hoc
shadowed_data <- apply_shadows(dat, shadows)
```

## Monte Carlo Framework

Evaluate estimator performance under various conditions:

``` r
# Define estimator
ipw_estimator <- function(data) {
  # Fit propensity score model
  ps_model <- glm(t1_a ~ b1 + b2 + t0_l, 
                  data = data, 
                  family = binomial)
  ps <- predict(ps_model, type = "response")
  
  # Calculate weights
  weights <- ifelse(data$t1_a == 1, 1/ps, 1/(1-ps))
  
  # Outcome model
  fit <- lm(t2_y ~ t1_a, weights = weights, data = data)
  
  list(
    estimate = coef(fit)["t1_a"],
    se = sqrt(diag(vcov(fit)))["t1_a"],
    converged = TRUE
  )
}

# Run simulation
results <- margot_monte_carlo(
  n_reps = 500,
  n_per_rep = 1000,
  dgp_params = list(
    waves = 2,
    params = list(a_lag_y_coef = 0.3)  # True effect
  ),
  shadows = list(me_shadow, miss_shadow),
  estimator_fn = ipw_estimator,
  truth_fn = function(data) 0.3,
  parallel = TRUE,
  n_cores = 4
)

# View results
print(results)
plot(results, type = "histogram")
```

### Performance Metrics

The framework automatically calculates: - Bias and relative bias -
Variance and MSE - Coverage of confidence intervals - Convergence
rates - Sample size retention

## Complete Example

``` r
# Compare estimators under measurement error
comparison <- example_measurement_error_comparison()

# Full workflow demonstration
results <- example_complete_workflow()
```

## Advanced Usage

### Custom Shadows

Create your own shadow types:

``` r
# Define apply method
apply_shadow.my_custom_shadow <- function(data, shadow, ...) {
  # Your shadowing logic here
  data
}

# Use it
shadow <- structure(
  list(type = "my_custom", params = list(...)),
  class = c("my_custom_shadow", "margot_shadow")
)
```

### Flexible Distributions

``` r
# Non-normal baseline
gamma_dist <- create_distribution(
  "gamma",
  params = list(shape = 2, rate = 1)
)

# Use in simulation
dat <- margot_simulate_flex(
  n = 1000,
  distributions = list(baseline = gamma_dist)
)
```

## Documentation

For detailed documentation, see:

``` r
# Package documentation
?margot.sim

# Key functions
?create_shadow
?margot_monte_carlo
?margot_simulate

# Examples
example(create_shadow)
example(margot_monte_carlo)
```

## Contributing

Contributions are welcome! Please:

1.  Fork the repository
2.  Create a feature branch
3.  Add tests for new functionality
4.  Submit a pull request

## License

MIT License

## Citation

If you use margot.sim in your research, please cite:

## Note

Observational data is often a poor reflection of reality. The
`margot.sim` package provides a simulation framework to quantify how
statistical estimators perform when the data they are fed are *shadows*
of a true causal process. This concept is nods to Plato’s Allegory of
the Cave, where prisoners mistake shadows for real forms (Bloom, Kirsch,
et al. 1968). `margot.sim` allows researchers to generate data from a
known causal truth and then systematically distort it with “shadows” to
see how well an estimator can glimpse the ground truth (or underlying
forms).

The framework’s power comes from its consistent architecture. Recurring
observational challenges—such as item-level missingness that creates
more realistic missing data patterns, and positivity violations that
test the boundaries of causal identification—are both implemented as
distinct shadows. This modular design maintains \*\*a clean separation
between the true data-generating process and the distorted data that
investigators observe, allowing for principled evaluation of statistical
methods in the face of real-world data limitations (Bulbulia 2024).

## References

``` r
citation("margot.sim")
```

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-bloom1968republic" class="csl-entry">

Bloom, Allan, Adam Kirsch, et al. 1968. *The Republic of Plato*. Vol. 2.
Basic Books New York.

</div>

<div id="ref-bulbulia2024wierd" class="csl-entry">

Bulbulia, J. A. 2024. “Methods in Causal Inference Part 3: Measurement
Error and External Validity Threats.” *Evolutionary Human Sciences* 6:
e42. <https://doi.org/10.1017/ehs.2024.33>.

</div>

</div>
