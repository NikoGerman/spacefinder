
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spacefinder

<!-- badges: start -->

<!-- badges: end -->

Spacefinder provides functionality to identify best performing minimum
volume subspaces within the hyperparameter space.

## Installation

You can install the development version of spacefinder from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("NikoGerman/spacefinder")
```

## Example

``` r
library(spacefinder)

DT <- data.table::data.table(
      task = sample(c("T1", "T2", "T3"), 300, replace = TRUE),
      auc = sample(runif(30), 100, replace = TRUE),
      hp1 = rnorm(300),
      hp2 = rnorm(300),
      hp3 = rnorm(300),
      cat_hp = sample(c("A", "B"), 300, replace = TRUE)
    )

tsk <- as_task_subspace(DT, auc ~ (hp1 + hp2 + hp3) * cat_hp)

learner <- LearnerSubspaceBox$new(tsk)

# train with slack
learner$train(lambda = .05)
```

``` r
summary(learner)
#> SUMMARY
#> --------------------------
#> Property                      Value         
#> ----------------------------  --------------
#> Target Measure                auc           
#> Numeric Hyperparameters       hp1, hp2, hp3 
#> Categorical Hyperparameters   cat_hp        
#> 
#> 
#> Coefficients:
#> --------------------------
#> cat_hp   hyperparameter           min          max
#> -------  ---------------  -----------  -----------
#> B        hp1               -0.5503593    1.1163089
#> B        hp2               -0.6611005    1.0055666
#> B        hp3               -0.5901556    1.0765107
#> A        hp1               -0.2317292    1.1968421
#> A        hp2               -1.2437949    0.2622605
#> A        hp3               -1.4374940   -0.0089225
#> 
#> 
#> Status:
#> --------------------------
#> cat_hp   status     objective_value   n_violations   observations
#> -------  --------  ----------------  -------------  -------------
#> B        optimal          0.5595411              6              6
#> A        optimal          0.4877062              5              7
```

``` r
ggplot2::autoplot(learner, wrap = TRUE)$A
```

<img src="man/figures/README-autoplot-1.png" width="100%" />

``` r
outliers(learner)
#>       task       auc        hp1        hp2        hp3 cat_hp
#>     <char>     <num>      <num>      <num>      <num> <char>
#>  1:     T2 0.9758080  0.1528895  0.7124344 -0.6401940      B
#>  2:     T2 0.9758080  1.0304430  1.4454659 -0.2052234      B
#>  3:     T1 0.9758080  2.1619531 -0.1031801 -0.5485984      B
#>  4:     T1 0.9758080 -0.3870489  0.8094467  1.7111854      B
#>  5:     T3 0.9467496  0.8141827 -1.9426559  0.4616530      B
#>  6:     T2 0.9758080 -1.3130385  0.5967580 -0.4748226      B
#>  7:     T3 0.9758080 -0.1446649  0.2622605 -2.0713723      A
#>  8:     T1 0.9758080  0.7625424  1.3436914  1.4339122      A
#>  9:     T2 0.9758080 -1.0786210  0.8339320 -0.9604514      A
#> 10:     T1 0.9758080  1.2739246 -0.7679298 -0.2958293      A
#> 11:     T2 0.9758080 -0.5657344 -2.2768801 -1.2326940      A
```
