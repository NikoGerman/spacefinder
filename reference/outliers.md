# Extract outlier configurations

Extracts hyperparameter configurations identified as outliers during
subspace fitting. Outliers are configurations that violated the subspace
constraints and were excluded via slack variables during regularized
optimization.

## Usage

``` r
outliers(object, ...)
```

## Arguments

- object:

  A trained `LearnerSubspace` object (or subclass) with fitted subspace
  parameters

- ...:

  Additional arguments (currently unused)

## Value

A `data.table` containing the outlier configurations with all columns
from the original task data. Returns an empty `data.table` if no
outliers were identified. A message is printed when no outliers exist.

## Details

**When Outliers Exist:**

Outliers are only identified when training with `lambda > 0`. The
regularization parameter allows the optimization to exclude some
configurations from the fitted subspace by introducing slack variables.

For a configuration to be considered an outlier, its slack variable must
exceed the threshold of \\10^{-5}\\.

When `lambda = NULL`, all configurations are forced to fit within the
subspace (hard constraints), so no outliers exist.

**Categorical Hyperparameters:**

When the task includes categorical hyperparameters, outliers are
identified separately for each categorical level and combined in the
returned `data.table`.

**Interpretation:**

Outliers typically represent:

- Configurations in sparse regions of hyperparameter space

- Anomalous configurations with unusual performance

- Configurations that don't fit the dominant subspace pattern

The number and characteristics of outliers can guide decisions about:

- Adjusting the `lambda` parameter

- Investigating unusual configurations

- Understanding the geometry of high-performing regions

## See also

[`LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
for the base learner class.
[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for axis-aligned hyperrectangles.
[`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md)
for oriented hyperrectangles.
[`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md)
for ellipsoids.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train with regularization (allows outliers)
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9, lambda = 0.1)

# Extract outliers
outlier_configs <- outliers(learner)
print(nrow(outlier_configs))  # Number of outliers
print(outlier_configs)  # View outlier configurations

# Check outlier information from result
print(learner$result$n_violations)  # Total number of outliers
print(learner$result$outliers)  # Outlier indices

# Train without regularization (no outliers)
learner$train(q_val = 0.9, lambda = NULL)
outlier_configs <- outliers(learner)  # Returns empty data.table

# With categorical hyperparameters
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"),
                         cat_hps = "optimizer")
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9, lambda = 0.1)
outlier_configs <- outliers(learner)  # Combined across all levels
} # }
```
