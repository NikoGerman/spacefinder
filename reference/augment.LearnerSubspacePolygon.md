# Augment oriented hyperrectangle learner with beta densities

Transforms data from the fitted oriented (rotated) hyperrectangle
subspace to the unit hypercube and fits univariate beta distributions to
each hyperparameter dimension using weighted maximum likelihood
estimation. The polygon learner uses the L-infinity norm, so the fitted
subspace is \\\\Ax + b\\\_\infty \leq 1\\, which maps to \\\[-1, 1\]^d\\
before being rescaled to \\\[0, 1\]^d\\ for beta fitting.

## Usage

``` r
# S3 method for class 'LearnerSubspacePolygon'
augment(x, regularize = TRUE, ...)
```

## Arguments

- x:

  A `LearnerSubspacePolygon` object with fitted subspace parameters

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

1.  Transform data: \\z = A^{-1}(y - b)\\ to map to \\\[-1, 1\]^d\\

2.  Rescale to unit cube: \\x = (z + 1) / 2\\ to map to \\\[0, 1\]^d\\

3.  Filter points where all coordinates lie in \\\[0, 1\]^d\\

4.  Fit \\Beta(\alpha, \beta)\\ to each dimension via weighted MLE

5.  Apply regularization if requested: \\\alpha, \beta \geq 1\\

The key difference from the Box learner is that oriented hyperrectangles
use the L-infinity norm \\\\z\\\_\infty \leq 1\\, which defines a
hypercube in the transformed space \\\[-1, 1\]^d\\. We rescale this to
\\\[0, 1\]^d\\ before fitting beta distributions.

**Matrix Inversion:**

Since \\A\\ is a general positive definite matrix (not diagonal), uses
Cholesky decomposition for efficient and stable inversion: \\A^{-1} =
(R^T R)^{-1} = R^{-1}R^{-T}\\ where \\R\\ is the Cholesky factor.

**Weights:**

Data points are weighted by their performance (target measure values),
normalized to sum to 1. This gives more influence to higher-performing
configurations.

**Regularization:**

When `regularize = TRUE` (default), ensures \\\alpha \geq 1\\ and
\\\beta \geq 1\\. This prevents U-shaped densities and ensures the mode
exists in the interior of \\\[0,1\]\\.

**Categorical Hyperparameters:**

When the task includes categorical hyperparameters, separate beta
distributions are fitted for each combination of hyperparameter and
categorical level.

## See also

[`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md)
for the learner class.
[`fit_beta_mle_single`](https://nikogerman.github.io/spacefinder/reference/fit_beta_mle_single.md)
for the univariate beta MLE algorithm.
[`augment.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/augment.LearnerSubspaceBox.md)
for the axis-aligned variant.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train learner
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspacePolygon$new(task)
learner$train(q_val = 0.9, lambda = 0.1)

# Fit beta densities
densities <- augment(learner)
print(densities)

# Without regularization
densities_unreg <- augment(learner, regularize = FALSE)
} # }
```
