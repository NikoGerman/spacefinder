# Subspace learner base class

Abstract R6 base class for learning hyperparameter subspaces that
contain high-quality configurations. Provides a unified framework for
fitting geometric regions (hyperrectangles, ellipsoids) to
top-performing hyperparameter configurations identified by a quantile
threshold.

## Details

**Overview:**

Subspace learners identify promising regions in hyperparameter space by:

1.  Filtering configurations to top quantile based on performance
    measure

2.  Fitting a geometric subspace (implementation-specific) to filtered
    data

3.  Optionally allowing outliers via regularization parameter `lambda`

**Geometric Representations:**

All learners represent subspaces via transformation \\y = Ax + b\\
where:

- \\x \in \[0,1\]^p\\ are unit cube coordinates

- \\y \in \mathbb{R}^p\\ are original hyperparameter coordinates

- \\A \in \mathbb{R}^{p \times p}\\ defines shape and orientation

- \\b \in \mathbb{R}^p\\ is the translation vector

Different learner types impose different structure on matrix \\A\\:

- **Box**: \\A\\ is diagonal (axis-aligned hyperrectangle)

- **Polygon**: \\A\\ is general positive definite (oriented
  hyperrectangle)

- **Ellipsoid**: \\A\\ is general positive definite (full ellipsoid)

**Categorical Hyperparameters:**

When the task includes categorical hyperparameters, separate subspaces
are fitted for each categorical level independently. This allows
different geometries for different categories (e.g., different learning
rate ranges per optimizer).

**Regularization via Slack Variables:**

The `lambda` parameter controls the volume-outlier trade-off:

- `lambda = NULL`: Hard constraints, all points must fit inside

- `lambda > 0`: Soft constraints, allows outliers with penalty

- Larger `lambda`: Smaller subspaces, more outliers tolerated

- Smaller `lambda`: Larger subspaces, fewer outliers tolerated

**Workflow:**

    # 1. Create task
    task <- SubspaceTask$new(data, target_measure = "accuracy")

    # 2. Initialize learner (use specific subclass)
    learner <- LearnerSubspaceBox$new(task)

    # 3. Train on top configurations
    learner$train(q_val = 0.9, lambda = 0.1)

    # 4. Extract fitted parameters
    coef(learner, vectorize = TRUE)

    # 5. Add density parameters
    augment(learner)

## See also

[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for axis-aligned hyperrectangles.
[`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md)
for oriented hyperrectangles.
[`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md)
for ellipsoids.
[`TaskSubspace`](https://nikogerman.github.io/spacefinder/reference/TaskSubspace.md)
for task definition.

## Public fields

- `task`:

  A TaskSubspace object

- `result`:

  Training result

- `top_configs`:

  Top hyperparameter configurations after quantile filtering

## Methods

### Public methods

- [`LearnerSubspace$new()`](#method-LearnerSubspace-new)

- [`LearnerSubspace$train()`](#method-LearnerSubspace-train)

- [`LearnerSubspace$clone()`](#method-LearnerSubspace-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new learner instance

#### Usage

    LearnerSubspace$new(task)

#### Arguments

- `task`:

  A `TaskSubspace` object

------------------------------------------------------------------------

### Method `train()`

Train the learner on top-quantile configurations

#### Usage

    LearnerSubspace$train(
      q_val = 1,
      lambda = NULL,
      tasks = NULL,
      exclude_tasks = NULL
    )

#### Arguments

- `q_val`:

  Quantile threshold for filtering configurations (0-1)

- `lambda`:

  Regularization parameter for slack variables (default = NULL)

- `tasks`:

  Character vector of task names to include (optional)

- `exclude_tasks`:

  Character vector of task names to exclude (optional)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerSubspace$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an abstract class - use specific implementations

# Create task
task <- SubspaceTask$new(
  data = benchmark_data,
  target_measure = "auc",
  cat_hps = "optimizer"
)

# Use Box learner (axis-aligned)
learner_box <- LearnerSubspaceBox$new(task)
learner_box$train(q_val = 0.9, lambda = 0.1)

# Use Ellipsoid learner (most flexible)
learner_ellip <- LearnerSubspaceEllipsoid$new(task)
learner_ellip$train(q_val = 0.95, lambda = NULL)

# Filter specific tasks
learner_box$train(
  q_val = 0.8,
  tasks = c("task1", "task2"),
  lambda = 0.05
)
} # }
```
