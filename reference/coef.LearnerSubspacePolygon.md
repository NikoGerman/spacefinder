# Extract coefficients from fitted oriented hyperrectangle learner

Extracts fitted subspace parameters from a trained
`LearnerSubspacePolygon` object. Returns the transformation matrix \\A\\
and translation vector \\b\\ that define the fitted oriented
hyperrectangle.

## Usage

``` r
# S3 method for class 'LearnerSubspacePolygon'
coef(object, ...)
```

## Arguments

- object:

  A `LearnerSubspacePolygon` object with fitted subspace parameters

- ...:

  Additional arguments (currently unused)

## Value

A `data.table` with columns:

- `hyperparameters`: List column containing hyperparameter names

- `A`: List column of positive definite matrices defining hyperrectangle
  shape and orientation

- `b`: List column of translation vectors

- `cat_hp`: Categorical level (only if task has categorical
  hyperparameters)

## Details

The oriented hyperrectangle is defined by \\\\Ax + b\\\_\infty \leq 1\\
where:

- \\A \in \mathbb{R}^{p \times p}\\ is a positive definite matrix

- \\b \in \mathbb{R}^p\\ is the translation vector

- \\x\\ are points inside the hyperrectangle in original coordinates

- The L-infinity norm maintains rectangular shape with sharp corners

The center of the hyperrectangle is: \\c = -A^{-1}b\\

Unlike `LearnerSubspaceBox` where \\A\\ is diagonal (axis-aligned), here
\\A\\ is a general positive definite matrix allowing arbitrary rotation
while maintaining the rectangular structure.

The eigendecomposition of \\A\\ reveals the orientation: if \\A =
V\Lambda V^T\\, then \\V\\ gives the principal directions and
\\\Lambda\\ the scaling along those directions.

When the task includes categorical hyperparameters, separate coefficient
sets are returned for each categorical level, identified by the `cat_hp`
column.

## Error Handling

Throws an error if the learner has not been trained. Call `train()`
before extracting coefficients.

## See also

[`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md)
for the learner class.
[`coef.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspaceBox.md)
for the axis-aligned variant.
[`coef.LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspaceEllipsoid.md)
for the ellipsoid variant.
[`augment.LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/augment.LearnerSubspacePolygon.md)
for adding density parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train learner
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspacePolygon$new(task)
learner$train(q_val = 0.9, lambda = 0.1)

# Extract coefficients
coefs <- coef(learner)
print(coefs$A[[1]])  # Shape matrix (not diagonal)
print(coefs$b[[1]])  # Translation vector

# Compute hyperrectangle center
A <- coefs$A[[1]]
b <- coefs$b[[1]]
center <- -solve(A) %*% b

# Analyze orientation
eigen_decomp <- eigen(A)
principal_directions <- eigen_decomp$vectors
scalings <- eigen_decomp$values
} # }
```
