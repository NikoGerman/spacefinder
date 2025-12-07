# Create a subspace task

Convenience function to create a `TaskSubspace` object. This is a
wrapper around `TaskSubspace$new()`.

## Usage

``` r
as_task_subspace(...)
```

## Arguments

- ...:

  Arguments passed to
  [`TaskSubspace`](https://nikogerman.github.io/spacefinder/reference/TaskSubspace.md)`$new()`.
  See
  [`TaskSubspace`](https://nikogerman.github.io/spacefinder/reference/TaskSubspace.md)
  for details on available parameters.

## Value

A `TaskSubspace` object

## See also

[`TaskSubspace`](https://nikogerman.github.io/spacefinder/reference/TaskSubspace.md)
for full documentation and examples.

## Examples

``` r
if (FALSE) { # \dontrun{
# Explicit specification
task <- as_task_subspace(
  data = benchmark_data,
  target_measure = "auc",
  hps = c("learning_rate", "max_depth")
)

# Formula interface
task <- as_task_subspace(
  data = benchmark_data,
  formula = auc ~ (learning_rate + max_depth) * optimizer
)
} # }
```
