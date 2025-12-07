# Create convex hull pairwise plots for oriented hyperrectangles

Helper function that generates pairwise scatter plots with convex hull
projections of oriented hyperrectangles.

## Usage

``` r
create_chull_pairwise_plots(
  coefficients,
  hps,
  top_configs,
  data,
  n_points,
  size_top,
  size_all,
  wrap,
  cat_hps = NULL,
  level = NULL,
  ...
)
```

## Arguments

- coefficients:

  Coefficient data.table from coef(learner, vectorize = TRUE)

- hps:

  Character vector of hyperparameter names to plot

- top_configs:

  data.table of top-performing configurations

- data:

  data.table of all configurations

- n_points:

  Number of points for polygon visualization (unused, kept for
  compatibility)

- size_top:

  Point size for top configurations

- size_all:

  Point size for all data points

- wrap:

  Logical indicating whether to wrap plots

- cat_hps:

  Name of categorical hyperparameter (if any)

- level:

  Current categorical level being plotted (if any)

- ...:

  Additional arguments passed to patchwork::wrap_plots()

## Value

If `wrap = TRUE`: A patchwork object combining all plots. If
`wrap = FALSE`: A list of ggplot objects. For univariate case: A single
ggplot histogram.
