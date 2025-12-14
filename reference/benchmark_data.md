# Synthetic Hyperparameter Benchmark Data

Simulated hyperparameter tuning results designed to demonstrate
spacefinder functionality. Contains performance metrics (AUC) for
various hyperparameter configurations across multiple tasks with three
distinct performance patterns and task-specific hyperparameter ranges.

## Usage

``` r
benchmark_data
```

## Format

A `data.table` with 7500 rows and 5 columns:

- task:

  Character. Task identifier (task1 through task5)

- learning_rate:

  Numeric. Learning rate with task-specific ranges. Base range
  \\\[0.0001, 0.1\]\\ scaled by exponential random factor per task

- max_depth:

  Integer. Maximum tree depth with task-specific ranges. Base range
  \\\[2, 11\]\\ shifted by random offset (1-4) per task

- optimizer:

  Character. Optimizer type: "SGD", "Adam", or "RMSprop"

- auc:

  Numeric. Area Under ROC Curve in range \\\[0.5, 1.0\]\\. Higher is
  better, normalized per optimizer to span \\\[0.5, 1.0\]\\

## Source

Synthetically generated for package examples and vignettes

## Details

The data is synthetically generated to showcase different subspace
learner strengths through three distinct performance patterns, each
mapped to a realistic optimizer name:

**Optimizer Patterns (in normalized hyperparameter space):**

- **Adam**: Unimodal peaked pattern with performance concentrated at the
  center (0.5, 0.5) of the normalized hyperparameter space. Performance
  decays exponentially with distance from this peak.

- **SGD**: Linear dependency pattern where performance follows the
  diagonal (learning_rate \\\approx\\ max_depth after normalization).
  Performance is \\1 - \|a - b\|\\ where \\a, b \in \[0,1\]\\.

- **RMSprop**: Bimodal pattern with two Gaussian peaks centered at (0.2,
  0.2) and (0.8, 0.8) in normalized space.

**Task-Specific Variations:**

Each of the five tasks has different optimal hyperparameter ranges:

- Learning rates are scaled by task-specific exponential random factors

- Tree depths are shifted by task-specific random offsets (1-4)

- These range differences simulate realistic scenarios where different
  datasets require different hyperparameter scales while maintaining the
  same underlying performance patterns

Performance values are normalized per optimizer to ensure all patterns
span \\\[0.5, 1.0\]\\, with added Gaussian noise (\\\sigma = 0.02\\) to
simulate realistic evaluation variability.

## Examples

``` r
data(benchmark_data)
head(benchmark_data)
#>      task learning_rate max_depth optimizer       auc
#>    <char>         <num>     <num>    <char>     <num>
#> 1:  task1  1.669706e-03        11       SGD 0.8732184
#> 2:  task1  7.156343e-04        10       SGD 0.8407793
#> 3:  task1  3.214906e-03         8       SGD 0.9680098
#> 4:  task1  5.028091e-05         7       SGD 0.7771257
#> 5:  task1  1.855155e-03        12       SGD 0.8427566
#> 6:  task1  2.585816e-03         3       SGD 0.7729911

# Summary statistics by optimizer
benchmark_data[, .(
  n = .N,
  mean_auc = mean(auc),
  max_auc = max(auc)
), by = optimizer]
#>    optimizer     n  mean_auc max_auc
#>       <char> <int>     <num>   <num>
#> 1:       SGD  2500 0.8346208       1
#> 2:      Adam  2500 0.6010436       1
#> 3:   RMSprop  2500 0.6884296       1


# Visualize patterns for a specific task
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(benchmark_data[task == "task1"],
       aes(log10(learning_rate), max_depth, color = auc)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(limits = c(0.5, 1)) +
  facet_wrap(~optimizer) +
  theme_minimal() +
  labs(
    title = "Performance Patterns by Optimizer (Task 1)",
    subtitle = "Adam: peaked | SGD: linear | RMSprop: bimodal"
  )
} # }
```
