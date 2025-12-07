# Axis-aligned hyperrectangle subspace learner

Learns axis-aligned hyperrectangles (boxes with edges parallel to
coordinate axes) that contain high-quality hyperparameter
configurations.

## Details

**Geometry:**

Fits a hyperrectangle defined by independent bounds per dimension: \$\$S
= \\x \in \mathbb{R}^p : l_i \leq x_i \leq u_i\\\$\$

The transformation to unit hypercube is \\y = Ax + b\\ where:

- \\A = \text{diag}(1/(u_i - l_i))\\ is diagonal (independent scaling)

- \\b = -l/(u - l)\\ is the translation vector

- \\x \in \[l, u\]\\ are points in the fitted subspace

- \\y \in \[0, 1\]^p\\ are points in the unit hypercube

**Optimization (`lambda` specified):** \$\$\min\_{l,u,\xi}
\frac{\lambda}{2}\\u-l\\\_2^2 + \frac{1}{2n}\sum_t(\xi_t^- +
\xi_t^+)\$\$ subject to: \\l - \xi_t^- \leq x^{(t)} \leq u + \xi_t^+\\,
\\\xi_t^-, \xi_t^+ \geq 0\\

Regularization parameter \\\lambda\\ controls subspace size: larger
values produce smaller subspaces with more outliers. Slack variables
\\\xi_t^-, \xi_t^+\\ allow configurations to violate bounds.

Uses ECOS solver. Special handling for univariate case (\\p=1\\).

**Simple mode (`lambda = NULL`):**

Uses coordinate-wise min/max: \\l_i = \min_t x_i^{(t)}\\, \\u_i = \max_t
x_i^{(t)}\\

No outliers are possible in this mode (hard constraints).

**Key Properties:**

- Most interpretable: bounds directly specify valid ranges

- Fastest: O(np) with diagonal matrices

- Best for independent hyperparameters

- Cannot capture correlations between hyperparameters

- Transformation matrix A is always diagonal

**Comparison with other learners:**

- **Box**: Axis-aligned only, fastest, most interpretable

- **Polygon**: Can rotate (L-infinity norm), maintains sharp corners

- **Ellipsoid**: Can rotate (L2 norm), smooth boundaries, most flexible

## See also

[`LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
for inherited methods and general workflow.
[`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md)
for oriented hyperrectangles.
[`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md)
for ellipsoids.
[`coef.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspaceBox.md)
for extracting fitted parameters.
[`augment.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/augment.LearnerSubspaceBox.md)
for adding density parameters.

## Super class

[`spacefinder::LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
-\> `LearnerSubspaceBox`

## Methods

### Public methods

- [`LearnerSubspaceBox$clone()`](#method-LearnerSubspaceBox-clone)

Inherited methods

- [`spacefinder::LearnerSubspace$initialize()`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.html#method-initialize)
- [`spacefinder::LearnerSubspace$train()`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.html#method-train)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSubspaceBox$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create task and learner
task <- TaskSubspace$new(data, target_measure = "accuracy",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspaceBox$new(task)

# Simple min/max bounds (no regularization)
learner$train(q_val = 0.9, lambda = NULL)
coef(learner)

# Regularized optimization
learner$train(q_val = 0.9, lambda = 0.1)
result <- learner$result
print(result$coefficients)  # Bounds per hyperparameter
print(result$n_violations)  # Number of outliers
print(result$outliers)      # Outlier indices

# With categorical hyperparameters
task <- TaskSubspace$new(data, target_measure = "accuracy",
                         hps = c("learning_rate", "max_depth"),
                         cat_hps = "optimizer")
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.95, lambda = 0.05)
coef(learner)  # Returns separate bounds per optimizer

# Get transformation matrices (y = Ax + b)
coef(learner, vectorize = TRUE)  # Returns A (diagonal), b

# Check which configs are in the subspace
bounds <- coef(learner)
in_subspace <- learner$task$data[
  learning_rate >= bounds[hyperparameter == "learning_rate"]$min &
  learning_rate <= bounds[hyperparameter == "learning_rate"]$max &
  max_depth >= bounds[hyperparameter == "max_depth"]$min &
  max_depth <= bounds[hyperparameter == "max_depth"]$max
]
} # }
```
