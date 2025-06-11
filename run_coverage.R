# Script to run code coverage locally

# Install covr if needed
if (!requireNamespace("covr", quietly = TRUE)) {
  install.packages("covr")
}

# Run coverage for the entire package
library(covr)

# Basic coverage
coverage <- package_coverage()

# Print coverage summary
print(coverage)

# Get coverage percentage
cat("\nOverall coverage:", percent_coverage(coverage), "%\n")

# Generate an interactive HTML report
report(coverage)

# Or if you want to see which lines are not covered
zero_coverage(coverage)

# To run coverage and send to codecov (requires token for private repos)
# codecov(coverage)