# Visualize fitted axis-aligned hyperrectangles

Creates pairwise scatter plots showing fitted axis-aligned
hyperrectangles overlaid on hyperparameter configurations. Displays both
the top-performing configurations used for fitting and all data points
for context.

## Usage

``` r
# S3 method for class 'LearnerSubspaceBox'
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

  A trained `LearnerSubspaceBox` object with fitted subspace

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

- Blue rectangle: Fitted axis-aligned hyperrectangle (box bounds)

For \\p\\ selected hyperparameters, creates \\\binom{p}{2}\\ pairwise
plots.

**Categorical Hyperparameters:**

When the task includes a categorical hyperparameter, separate plots are
created for each categorical level, showing the corresponding fitted
box.

**Interactive Prompt:**

When plotting more than 3 hyperparameters with wrapping enabled, the
function prompts for confirmation due to potential readability issues.
Use `force = TRUE` to bypass this prompt.

**Dependencies:**

Requires `ggplot2`. If `wrap = TRUE`, also requires `patchwork`.

## See also

[`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md)
for the learner class.
[`coef.LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/coef.LearnerSubspaceBox.md)
for extracting fitted bounds.

## Examples

``` r
if (FALSE) { # \dontrun{
# Train learner
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"))
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9, lambda = 0.1)

# Plot all hyperparameters (wrapped)
autoplot(learner)

# Plot specific hyperparameters
autoplot(learner, select = c("learning_rate", "max_depth"))

# Get individual plots without wrapping
plots <- autoplot(learner, wrap = FALSE)
plots[[1]]  # First pairwise plot

# Customize wrapping layout
autoplot(learner, ncol = 2, guides = "collect")

# With categorical hyperparameters
task <- TaskSubspace$new(data, target_measure = "auc",
                         hps = c("learning_rate", "max_depth"),
                         cat_hps = "optimizer")
learner <- LearnerSubspaceBox$new(task)
learner$train(q_val = 0.9)
autoplot(learner)  # Separate plots per optimizer
} # }
```
