# Ellipsoidal subspace learner

Learns minimum-volume ellipsoids that contain high-quality
hyperparameter configurations. Most flexible geometry with smooth
boundaries.

## Details

**Geometry:**

Fits an ellipsoid: \\E = \\x \in \mathbb{R}^p : \\Ax + b\\\_2 \leq 1\\\\

Matrix \\A \in \mathbb{R}^{p \times p}\\ is positive definite. Center at
\\c = -A^{-1}b\\. Semi-axes determined by eigenvalues of \\A^{-1}\\.

**Optimization (`lambda` specified):** \$\$\min\_{A \succeq 0, b, s}
\lambda \cdot (-\log\det(A)) + \frac{1}{n}\sum\_{t=1}^n s_t\$\$ subject
to: \\\\Ax^{(t)} + b\\\_2 \leq 1 + s_t\\, \\s_t \geq 0\\

Uses SCS solver for semidefinite programming. Volume minimization via
\\-\log\det(A)\\. The L2 norm creates smooth ellipsoidal boundaries.

**Simple mode (`lambda = NULL`):**

Minimizes volume without slack variables. All points must satisfy
\\\\Ax^{(t)} + b\\\_2 \leq 1\\.

**Key Properties:**

- Most flexible: arbitrary rotations and scaling

- Smooth boundaries (no corners)

- Optimal for normally distributed data

- Most expensive: \\O(p^3)\\ scaling

- For \\p \> 50\\, consider Box or Polygon learners

**Comparison with other learners:**

- **Box**: Axis-aligned, rectangular, fastest

- **Polygon**: Can rotate, rectangular shape, intermediate cost

- **Ellipsoid**: Can rotate, smooth boundaries, most flexible, slowest

## Note

This learner does not implement an `augment()` method.

## See also

[`LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
for inherited methods and general workflow.
[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for axis-aligned hyperrectangles (faster).
[`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md)
for oriented hyperrectangles (intermediate).
[`coef.LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspaceEllipsoid.md)
for extracting fitted parameters.

## Super class

[`spacefinder::LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
-\> `LearnerSubspaceEllipsoid`

## Methods

### Public methods

- [`LearnerSubspaceEllipsoid$clone()`](#method-LearnerSubspaceEllipsoid-clone)

Inherited methods

- [`spacefinder::LearnerSubspace$initialize()`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.html#method-initialize)
- [`spacefinder::LearnerSubspace$train()`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.html#method-train)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSubspaceEllipsoid$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create task and learner
task <- SubspaceTask$new(data, target_measure = "accuracy")
learner <- LearnerSubspaceEllipsoid$new(task)

# Minimum-volume ellipsoid (hard constraints)
learner$train(lambda = NULL)
result <- learner$result
print(result$A)
print(result$b)

# Regularized optimization (allows outliers)
learner$train(q_val = 0.9, lambda = 0.1)
result <- learner$result
print(result$n_violations)
print(result$outliers)

# Inspect ellipsoid geometry
center <- -solve(result$A) %*% result$b
eigendecomp <- eigen(solve(result$A))
semi_axes <- sqrt(eigendecomp$values)
print(semi_axes)

# With categorical hyperparameters
task <- SubspaceTask$new(data, target_measure = "accuracy",
                         cat_hps = "optimizer")
learner <- LearnerSubspaceEllipsoid$new(task)
learner$train(q_val = 0.95, lambda = 0.05)
coef(learner, vectorize = TRUE)
} # }
```
