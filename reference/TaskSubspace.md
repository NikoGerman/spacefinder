# Subspace task definition

R6 class for defining hyperparameter optimization tasks. Encapsulates
benchmark data with hyperparameter configurations and performance
measures, providing a standardized interface for subspace learners.

## Details

**Overview:**

A subspace task combines:

- Benchmark data with hyperparameter configurations and performance
  measures

- Specification of which columns are hyperparameters (continuous and
  categorical)

- Target performance measure to optimize

**Data Requirements:**

The input `data.table` must contain:

- A `task` column identifying different tasks/datasets

- A target measure column (e.g., "auc", "accuracy", "rmse")

- One or more numeric hyperparameter columns

- Optionally, one categorical hyperparameter column

**Initialization Modes:**

**Mode 1: Explicit specification**

    task <- TaskSubspace$new(
      data = benchmark_data,
      target_measure = "accuracy",
      hps = c("learning_rate", "max_depth"),
      cat_hps = "optimizer"
    )

**Mode 2: Formula interface**

    task <- TaskSubspace$new(
      data = benchmark_data,
      formula = accuracy ~ (learning_rate + max_depth) * optimizer
    )

The formula syntax is: `target ~ (hp1 + hp2 + ...) * cat_hp`

- Left-hand side: target measure

- Right-hand side before `*`: continuous hyperparameters

- Right-hand side after `*`: categorical hyperparameter (optional)

**Categorical Hyperparameters:**

Currently supports at most one categorical hyperparameter. When
specified, learners will fit separate subspaces for each categorical
level.

## See also

[`LearnerSubspace`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspace.md)
for the base learner class.
[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for axis-aligned hyperrectangles.
[`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md)
for oriented hyperrectangles.
[`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md)
for ellipsoids.

## Public fields

- `data`:

  A `data.table` containing hyperparameter configurations, performance
  measures, and task identifiers. Must have a `task` column.

- `target_measure`:

  Character string specifying the performance measure column name (e.g.,
  "auc", "accuracy", "rmse")

- `hps`:

  Character vector of continuous hyperparameter column names

- `cat_hps`:

  Character string specifying the categorical hyperparameter column name
  (optional, currently limited to one)

## Methods

### Public methods

- [`TaskSubspace$new()`](#method-TaskSubspace-new)

- [`TaskSubspace$clone()`](#method-TaskSubspace-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new task instance

#### Usage

    TaskSubspace$new(
      data,
      formula = NULL,
      target_measure = NULL,
      hps = NULL,
      cat_hps = NULL
    )

#### Arguments

- `data`:

  A data.table containing task performance data

- `formula`:

  Formula specification (optional)

- `target_measure`:

  Name of the performance measure column

- `hps`:

  Character vector of continuous hyperparameter names

- `cat_hps`:

  Character vector of categorical hyperparameter names (optional)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskSubspace$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Explicit specification
task <- TaskSubspace$new(
  data = benchmark_data,
  target_measure = "auc",
  hps = c("learning_rate", "max_depth", "min_samples_split")
)

# With categorical hyperparameter
task <- TaskSubspace$new(
  data = benchmark_data,
  target_measure = "accuracy",
  hps = c("learning_rate", "max_depth"),
  cat_hps = "optimizer"
)

# Formula interface
task <- TaskSubspace$new(
  data = benchmark_data,
  formula = auc ~ (learning_rate + max_depth) * optimizer
)

# Use with learner
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9)
} # }
```
