# Visualize fitted ellipsoids

Creates pairwise scatter plots showing fitted ellipsoids overlaid on
hyperparameter configurations. Uses marginal ellipse projections to
visualize the smooth ellipsoidal boundaries in 2D pairwise plots.

## Usage

``` r
# S3 method for class 'LearnerSubspaceEllipsoid'
autoplot(
  object,
  select = "all",
  force = FALSE,
  wrap = TRUE,
  size_top = 0.7,
  size_all = 0.5,
  ...
)
```

## Arguments

- object:

  A trained `LearnerSubspaceEllipsoid` object with fitted subspace

- select:

  Character vector of hyperparameter names to plot, or "all" (default)
  to plot all hyperparameters

- force:

  Logical indicating whether to skip the confirmation prompt when
  plotting many hyperparameters. Default: `FALSE`

- wrap:

  Logical indicating whether to combine plots using
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Default: `TRUE`

- size_top:

  Numeric point size for top-performing configurations (orange crosses).
  Default: 0.7

- size_all:

  Numeric point size for all data points (gray background). Default: 0.5

- ...:

  Additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  (only used when `wrap = TRUE`)

## Value

If `wrap = TRUE`: A single patchwork object combining all plots.

If `wrap = FALSE` and no categorical hyperparameters: A list of ggplot
objects, one per hyperparameter pair.

If `wrap = FALSE` and categorical hyperparameters present: A named list
where each element is a list of ggplot objects for that categorical
level.

## Details

**Plot Structure:**

Each plot shows:

- Gray points: All configurations in the dataset (low alpha)

- Orange crosses: Top-performing configurations used for fitting

- Blue ellipse: Projection of the fitted ellipsoid onto 2D plane

- Blue diamond: Center of the ellipsoid

For \\p\\ selected hyperparameters, creates \\\binom{p}{2}\\ pairwise
plots.

**Visualization Method:**

The ellipsoid is defined by \\\\Ax + b\\\_2 \leq 1\\. For visualization:

1.  Computes center: \\c = -A^{-1}b\\

2.  Computes covariance: \\\Sigma = A^{-1}(A^{-1})^T\\

3.  For each 2D pair \\(i,j)\\, extracts marginal covariance
    \\\Sigma\_{ij}\\

4.  Uses Cholesky decomposition to transform unit circle to ellipse: \\x
    = L \theta + c\_{ij}\\ where \\L\\ is lower Cholesky factor of
    \\\Sigma\_{ij}\\ and \\\theta\\ are points on unit circle

The resulting ellipse represents the marginal distribution of the
ellipsoid projected onto the 2D hyperparameter pair, showing smooth
boundaries characteristic of ellipsoidal subspaces.

**Univariate Case:**

When only one hyperparameter is selected, displays a histogram with
vertical lines marking the ellipsoid boundaries (reduces to interval)
and rug plot for top configurations.

**Categorical Hyperparameters:**

When the task includes a categorical hyperparameter, separate plots are
created for each categorical level, showing the corresponding fitted
ellipsoid.

**Interactive Prompt:**

When plotting more than 3 hyperparameters with wrapping enabled, the
function prompts for confirmation due to potential readability issues.
Use `force = TRUE` to bypass this prompt.

**Dependencies:**

Requires `ggplot2` and `scales`. If `wrap = TRUE`, also requires
`patchwork`.

## See also

[`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md)
for the learner class.
[`coef.LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspaceEllipsoid.md)
for extracting fitted parameters.
[`autoplot.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/autoplot.LearnerSubspaceBox.md)
for axis-aligned visualization.
[`autoplot.LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/autoplot.LearnerSubspacePolygon.md)
for oriented hyperrectangle visualization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train learner
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspaceEllipsoid$new(task)
learner$train(q_val = 0.9, lambda = 0.1)

# Plot all hyperparameters (wrapped)
autoplot(learner)

# Plot specific hyperparameters
autoplot(learner, select = c("learning_rate", "max_depth"))

# Get individual plots without wrapping
plots <- autoplot(learner, wrap = FALSE)
plots[[1]]  # First pairwise plot

# Customize ellipse smoothness
autoplot(learner, n_points = 500)  # More points for smoother curves

# Customize wrapping layout
autoplot(learner, ncol = 2, guides = "collect")

# With categorical hyperparameters
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"),
                         cat_hps = "optimizer")
learner <- LearnerSubspaceEllipsoid$new(task)
learner$train(q_val = 0.9)
autoplot(learner)  # Separate plots per optimizer
} # }
```
