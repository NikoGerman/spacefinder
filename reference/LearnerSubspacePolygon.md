# Oriented hyperrectangle subspace learner

Learns oriented (rotated) hyperrectangles that contain high-quality
hyperparameter configurations. Allows arbitrary rotation while
maintaining rectangular shape.

## Details

**Geometry:**

Fits a rotated hyperrectangle defined by: \$\$\\x \in \mathbb{R}^p :
\\Ax + b\\\_\infty \leq 1\\\$\$

The transformation matrix \\A \in \mathbb{R}^{p \times p}\\ is positive
definite but not restricted to diagonal, allowing rotation in
hyperparameter space. The L-infinity norm constraint maintains
rectangular shape (sharp corners).

**Optimization (`lambda` specified):** \$\$\min\_{A \succeq 0, b, s}
\lambda \cdot (-\log\det(A)) + \frac{1}{n}\sum\_{t=1}^n s_t\$\$ subject
to: \\\\Ax^{(t)} + b\\\_\infty \leq 1 + s_t\\, \\s_t \geq 0\\

Uses SCS solver for semidefinite programming. Volume minimization via
\\-\log\det(A)\\.

**Simple mode (`lambda = NULL`):**

Minimizes volume without slack variables. All points must satisfy
\\\\Ax^{(t)} + b\\\_\infty \leq 1\\.

**Key Properties:**

- Intermediate flexibility: captures correlations between
  hyperparameters

- Maintains rectangular shape with sharp corners (unlike ellipsoids)

- More flexible than Box (can rotate), less flexible than Ellipsoid

- Computational cost between Box and Ellipsoid

- Good balance of interpretability and expressiveness

**Comparison with other learners:**

- **Box**: Axis-aligned, diagonal A, fastest, most interpretable

- **Polygon**: Can rotate, general A, rectangular shape, intermediate
  cost

- **Ellipsoid**: Can rotate, general A, smooth boundaries, slowest

## See also

[`LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
for inherited methods and general workflow.
[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for axis-aligned hyperrectangles (faster).
[`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md)
for ellipsoids (more flexible).
[`coef.LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspacePolygon.md)
for extracting fitted parameters.
[`augment.LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/augment.LearnerSubspacePolygon.md)
for adding density parameters.

## Super class

[`spacefinder::LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
-\> `LearnerSubspacePolygon`

## Methods

### Public methods

- [`LearnerSubspacePolygon$clone()`](#method-LearnerSubspacePolygon-clone)

Inherited methods

- [`spacefinder::LearnerSubspace$initialize()`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.html#method-initialize)
- [`spacefinder::LearnerSubspace$train()`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.html#method-train)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSubspacePolygon$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create task and learner
task <- SubspaceTask$new(data, target_measure = "accuracy")
learner <- LearnerSubspacePolygon$new(task)

# Minimum-volume oriented box (hard constraints)
learner$train(lambda = NULL)
result <- learner$result
print(result$A)  # Shape matrix (not diagonal)
print(result$b)  # Translation vector

# Regularized optimization (allows outliers)
learner$train(q_val = 0.9, lambda = 0.1)
result <- learner$result
print(result$n_violations)  # Number of outliers
print(result$outliers)      # Outlier indices

# Check orientation
eigendecomp <- eigen(result$A)
print(eigendecomp$vectors)  # Principal directions
print(1 / eigendecomp$values)  # Box widths along principal axes

# With categorical hyperparameters
task <- SubspaceTask$new(data, target_measure = "accuracy",
                         cat_hps = "optimizer")
learner <- LearnerSubspacePolygon$new(task)
learner$train(q_val = 0.95, lambda = 0.05)
coef(learner, vectorize = TRUE)  # Separate A, b per optimizer
} # }
```
