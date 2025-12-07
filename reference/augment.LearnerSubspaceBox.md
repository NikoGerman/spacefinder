# Augment axis-aligned box learner with beta densities

Transforms data from the fitted axis-aligned hyperrectangle subspace to
the unit hypercube and fits univariate beta distributions to each
hyperparameter dimension using weighted maximum likelihood estimation.
Since box learners use diagonal transformation matrices (independent
scaling), inversion is computationally efficient.

## Usage

``` r
# S3 method for class 'LearnerSubspaceBox'
augment(x, regularize = TRUE, ...)
```

## Arguments

- x:

  A `LearnerSubspaceBox` object with fitted subspace parameters

- regularize:

  Logical indicating whether to enforce `alpha >= 1` and `beta >= 1` to
  avoid U-shaped densities. Default: `TRUE`

- ...:

  Additional arguments passed to
  [`fit_beta_mle_single`](https://nikogerman.github.io/spacefinder/reference/fit_beta_mle_single.md)
  (e.g., `tol`, `max_iter`, `clip_eps`)

## Value

A `data.table` with columns:

- `parameter`: Hyperparameter name

- `alpha`: Fitted beta shape parameter (alpha \> 0)

- `beta`: Fitted beta shape parameter (beta \> 0)

- `converged`: Logical indicating whether MLE converged

- `iterations`: Number of Newton-Raphson iterations used

- `cat_hp`: Categorical level (only if task has categorical
  hyperparameters)

## Details

**Algorithm:**

For each categorical level (or globally if no categorical
hyperparameters):

1.  Extract fitted box bounds (min, max) for each hyperparameter

2.  Transform data to unit cube: \\x = (y - min) / (max - min)\\

3.  Filter points where all coordinates lie in \\\[0,1\]^d\\

4.  Fit \\Beta(\alpha, \beta)\\ to each dimension via weighted MLE

5.  Apply regularization if requested: \\\alpha, \beta \geq 1\\

If no valid points remain after filtering (shouldn't happen for box
learners with lambda = NULL), returns uniform prior \\Beta(1,1)\\ for
all dimensions.

**Weights:**

Data points are weighted by their performance (target measure values),
normalized to sum to 1. This gives more influence to higher-performing
configurations when fitting the beta distributions.

**Regularization:**

When `regularize = TRUE` (default), ensures \\\alpha \geq 1\\ and
\\\beta \geq 1\\. This prevents U-shaped densities (which occur when
both parameters are less than 1) and ensures the mode exists in the
interior of \\\[0,1\]\\.

**Categorical Hyperparameters:**

When the task includes categorical hyperparameters, separate beta
distributions are fitted for each combination of hyperparameter and
categorical level.

## See also

[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for the learner class.
[`fit_beta_mle_single`](https://nikogerman.github.io/spacefinder/reference/fit_beta_mle_single.md)
for the univariate beta MLE algorithm.
[`augment.LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/augment.LearnerSubspacePolygon.md)
for the oriented hyperrectangle variant.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train learner
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9)

# Fit beta densities
densities <- augment(learner)
print(densities)

# Without regularization (allows U-shaped densities)
densities_unreg <- augment(learner, regularize = FALSE)

# Sample from fitted distributions
n_samples <- 100
sampled_lr <- rbeta(n_samples,
                    densities[parameter == "learning_rate"]$alpha,
                    densities[parameter == "learning_rate"]$beta)
} # }
```
