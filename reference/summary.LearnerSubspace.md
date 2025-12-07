# Summarize fitted subspace learner

Provides a comprehensive summary of a trained subspace learner,
including task information, fitted coefficients, optimization status,
and outlier configurations. Prints formatted tables to the console and
invisibly returns the summary information.

## Usage

``` r
# S3 method for class 'LearnerSubspace'
summary(object, ...)
```

## Arguments

- object:

  A trained `LearnerSubspace` object (or subclass) with fitted subspace
  parameters

- ...:

  Additional arguments (currently unused)

## Value

Invisibly returns a list with components:

- `status`: `data.table` with optimization status, objective values,
  number of violations, and observation counts

- `coefficients`: `data.table` with fitted subspace parameters (format
  depends on learner type)

- `outliers`: `data.table` with outlier configurations (empty if no
  outliers or `lambda = NULL`)

## Details

**Printed Output:**

The function prints three formatted tables:

**1. Summary Table:**

- Target measure being optimized

- Numeric hyperparameters included in subspace

- Categorical hyperparameters (if any)

**2. Coefficients Table:**

- **Box**: min/max bounds per hyperparameter

- **Polygon/Ellipsoid**: A matrices and b vectors in list columns

- Separate rows for each categorical level (if applicable)

**3. Status Table:**

- `status`: Solver convergence status (e.g., "optimal", "solved")

- `objective_value`: Final objective function value

- `n_violations`: Number of configurations treated as outliers

- `observations`: Number of top configurations used for fitting

**Status Values:**

Fields may be `NULL` when:

- `lambda = NULL`: Simple min/max fitting (Box learner only)

- No outliers: All configurations fit within subspace

**Dependencies:**

Requires `knitr` package for formatted table output. If not available,
falls back to basic printing.

## See also

[`LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
for the base learner class. Methods
[`coef()`](https://rdrr.io/r/stats/coef.html) and `augment()` for
extracting fitted parameters.
[`outliers`](https://nikogerman.github.io/spacefinder/reference/outliers.md)
for extracting outlier configurations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train learner
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9, lambda = 0.1)

# Print summary to console
summary(learner)

# Capture summary information
info <- summary(learner)
print(info$status)
print(info$coefficients)
print(info$outliers)

# With categorical hyperparameters
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"),
                         cat_hps = "optimizer")
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9, lambda = 0.1)
summary(learner)  # Separate status rows per optimizer
} # }
```
