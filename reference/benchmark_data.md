# Synthetic Hyperparameter Benchmark Data

Simulated hyperparameter tuning results for demonstrating spacefinder
functionality. Contains performance metrics (AUC) for various
hyperparameter configurations across multiple tasks and optimizers.

## Usage

``` r
benchmark_data
```

## Format

A `data.table` with 1000 rows and 5 columns:

- task:

  Character. Task identifier (task1 through task5)

- learning_rate:

  Numeric. Learning rate in range \\\[0.0001, 0.1\]\\

- max_depth:

  Integer. Maximum tree depth in range \\\[3, 15\]\\

- optimizer:

  Character. Optimizer type: "SGD", "Adam", or "RMSprop"

- auc:

  Numeric. Area Under ROC Curve in range \\\[0.5, 0.98\]\\. Higher is
  better. Most values between 0.6-0.85, with top performers rarely
  exceeding 0.90

## Source

Synthetically generated for package examples

## Details

The data simulates realistic hyperparameter tuning scenarios where:

- Performance peaks around learning_rate \\0.003\\ and max_depth \\8\\

- Adam optimizer provides slight performance boost over SGD and RMSprop

- Each task has slight performance variations

- Most configurations are mediocre, with excellent performance being
  rare

Generated using a mixture of normal distributions with realistic noise
and soft capping to ensure values above 0.9 are extremely rare.

## Examples

``` r
data(benchmark_data)
head(benchmark_data)
#>      task learning_rate max_depth optimizer       auc
#>    <char>         <num>     <int>    <char>     <num>
#> 1:  task1  0.0555159953        10   RMSprop 0.5000000
#> 2:  task1  0.0647479824         4      Adam 0.5000000
#> 3:  task1  0.0007218029        11       SGD 0.9455143
#> 4:  task1  0.0309986570        14   RMSprop 0.8306492
#> 5:  task1  0.0084185357        13      Adam 0.9496575
#> 6:  task1  0.0036081771        15   RMSprop 0.9499609

# Summary statistics
summary(benchmark_data$auc)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.5000  0.6095  0.9329  0.8062  0.9498  0.9500 

# Create a task
task <- TaskSubspace$new(
  data = benchmark_data,
  target_measure = "auc",
  hps = c("learning_rate", "max_depth")
)
```
