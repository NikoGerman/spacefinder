# Extract coefficients from fitted axis-aligned box learner

Extracts fitted subspace parameters from a trained `LearnerSubspaceBox`
object. Returns either explicit hyperparameter bounds or the
transformation matrices that map the fitted axis-aligned hyperrectangle
to the unit hypercube.

## Usage

``` r
# S3 method for class 'LearnerSubspaceBox'
coef(object, vectorize = FALSE, ...)
```

## Arguments

- object:

  A `LearnerSubspaceBox` object with fitted subspace parameters

- vectorize:

  `logical` whether to return transformation matrices (`TRUE`) or
  explicit bounds (`FALSE`). Default: `FALSE`

  - `FALSE`: Returns `data.table` with columns `hyperparameter`, `min`,
    `max` for each hyperparameter

  - `TRUE`: Returns `data.table` with columns `hyperparameters` (list),
    `A` (diagonal matrix), `b` (translation vector)

- ...:

  Additional arguments (currently unused)

## Value

A `data.table` containing fitted subspace parameters.

**When `vectorize = FALSE` (explicit bounds):**

- `hyperparameter`: Hyperparameter name

- `min`: Lower bound of fitted interval

- `max`: Upper bound of fitted interval

- `cat_hp`: Categorical level (only if task has categorical
  hyperparameters)

**When `vectorize = TRUE` (transformation matrices):**

- `hyperparameters`: List column containing hyperparameter names

- `A`: List column of diagonal matrices with \\1/(max - min)\\ on
  diagonal

- `b`: List column of translation vectors equal to \\-min/(max - min)\\

- `cat_hp`: Categorical level (only if task has categorical
  hyperparameters)

## Details

For axis-aligned hyperrectangles, the transformation from the fitted
subspace to the unit hypercube \\\[0,1\]^d\\ is: \$\$y = Ax + b\$\$
where:

- \\A = diag(1/(max - min))\\ is a diagonal matrix (independent scaling
  per dimension)

- \\b = -min/(max - min)\\ is the translation vector

- \\x \in \[min, max\]^d\\ are original hyperparameter coordinates

- \\y \in \[0,1\]^d\\ are unit cube coordinates

This maps each hyperparameter from its fitted range \\\[min, max\]\\ to
\\\[0, 1\]\\.

When the task includes categorical hyperparameters, separate coefficient
sets are returned for each categorical level, identified by the `cat_hp`
column.

## Error Handling

Throws an error if the learner has not been trained. Call `train()`
before extracting coefficients.

## See also

[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for learner class.
[`coef.LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspacePolygon.md)
for the oriented hyperrectangle variant.
[`augment.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/augment.LearnerSubspaceBox.md)
for adding density parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train a box learner
learner <- LearnerSubspaceBox$new(task)
learner$train()

# Get explicit bounds
coef(learner)

# Get transformation matrices
coef(learner, vectorize = TRUE)
} # }
```
