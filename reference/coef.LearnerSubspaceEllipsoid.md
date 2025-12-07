# Extract coefficients from fitted ellipsoid learner

Extracts fitted subspace parameters from a trained
`LearnerSubspaceEllipsoid` object. Returns the transformation matrix
\\A\\ and translation vector \\b\\ that define the fitted ellipsoid.

## Usage

``` r
# S3 method for class 'LearnerSubspaceEllipsoid'
coef(object, ...)
```

## Arguments

- object:

  A `LearnerSubspaceEllipsoid` object with fitted subspace parameters

- ...:

  Additional arguments (currently unused)

## Value

A `data.table` with columns:

- `hyperparameters`: List column containing hyperparameter names

- `A`: List column of positive definite matrices defining ellipsoid
  shape and orientation

- `b`: List column of translation vectors

- `cat_hp`: Categorical level (only if task has categorical
  hyperparameters)

## Details

The ellipsoid is defined by \\\\Ax + b\\\_2 \leq 1\\ where:

- \\A \in \mathbb{R}^{p \times p}\\ is a positive definite matrix

- \\b \in \mathbb{R}^p\\ is the translation vector

- \\x\\ are points inside the ellipsoid in original coordinates

The center of the ellipsoid is: \\c = -A^{-1}b\\

The semi-axes lengths and orientations are determined by the
eigendecomposition of \\A^{-1}\\: if \\A^{-1} = V\Lambda V^T\\, then the
semi-axes have lengths \\\sqrt{\lambda_i}\\ in directions given by
columns of \\V\\.

When the task includes categorical hyperparameters, separate coefficient
sets are returned for each categorical level, identified by the `cat_hp`
column.

## Error Handling

Throws an error if the learner has not been trained. Call `train()`
before extracting coefficients.

## See also

[`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md)
for the learner class.
[`coef.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspaceBox.md)
for the axis-aligned variant.
[`coef.LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspacePolygon.md)
for the oriented hyperrectangle variant.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train learner
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspaceEllipsoid$new(task)
learner$train(q_val = 0.9, lambda = 0.1)

# Extract coefficients
coefs <- coef(learner)
print(coefs$A[[1]])  # Shape matrix
print(coefs$b[[1]])  # Translation vector

# Compute ellipsoid center
A <- coefs$A[[1]]
b <- coefs$b[[1]]
center <- -solve(A) %*% b

# Compute semi-axes
A_inv <- solve(A)
eigen_decomp <- eigen(A_inv)
semi_axes <- sqrt(eigen_decomp$values)
directions <- eigen_decomp$vectors
} # }
```
