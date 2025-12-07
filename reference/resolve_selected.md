# Resolve hyperparameter selection

Validates and resolves which hyperparameters to use based on the
selection argument and those present in the task. Handles both explicit
selection and the special "all" keyword.

## Usage

``` r
resolve_selected(object, select)
```

## Arguments

- object:

  A learner object containing a task with hyperparameters

- select:

  `character` vector of hyperparameter names to select, or the string
  "all" to select all available hyperparameters in the task

## Value

`character` vector of validated hyperparameter names that exist in the
task and were requested via `select`

## Details

When `select = "all"`, returns all hyperparameters in the task.
Otherwise, returns the intersection of requested hyperparameters and
those present in the task. Throws an error if none of the selected
hyperparameters exist in the task.

## Examples

``` r
if (FALSE) { # \dontrun{
# Select all hyperparameters
resolve_selected(learner, "all")

# Select specific hyperparameters
resolve_selected(learner, c("learning_rate", "max_depth"))
} # }
```
