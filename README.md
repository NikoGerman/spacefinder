
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
#> cat_hp   hyperparameter           min         max
#> -------  ---------------  -----------  ----------
#> B        hp1               -0.7764947   0.4734998
#> B        hp2               -0.8412868   1.5176070
#> B        hp3               -1.3116351   0.2921209
#> A        hp1               -0.8516437   0.1483579
#> A        hp2               -0.9486517   1.2107492
#> A        hp3               -0.5583463   1.0453884
#> 
#> 
#> Status:
#> --------------------------
#> cat_hp   status     objective_value   n_violations   observations
#> -------  --------  ----------------  -------------  -------------
#> B        optimal          0.5019506              5              8
#> A        optimal          0.5383008              7             10
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
ggplot2::autoplot(learner, wrap = TRUE)$A
```

<img src="man/figures/README-autoplot-1.png" width="100%" />

``` r
outliers(learner)
#>       task       auc           hp1        hp2        hp3 cat_hp
#>     <char>     <num>         <num>      <num>      <num> <char>
#>  1:     T1 0.9457825 -0.0574183463 -0.8412868  0.9874632      B
#>  2:     T1 0.9457825 -0.3108891819 -1.1474346  0.2921209      B
#>  3:     T3 0.9457825 -0.2442807584  0.6686297 -1.8293555      B
#>  4:     T2 0.9457825 -1.7166074293  1.7225320 -1.0560038      B
#>  5:     T3 0.9457825  1.9609021680  1.7007655 -1.3116351      B
#>  6:     T2 0.9457825  1.3581245233  0.5969794  0.3494849      A
#>  7:     T3 0.9039644 -0.4916394832  2.2959415 -0.5583463      A
#>  8:     T3 0.9039644  0.4589323361  1.7046237  0.3321837      A
#>  9:     T1 0.9457825 -1.3361378451  0.1975747  0.2458097      A
#> 10:     T1 0.9457825 -0.0002474957 -1.6072197  1.6925467      A
#> 11:     T3 0.9039644 -0.6039371069 -0.3122782 -2.4570447      A
#> 12:     T2 0.9457825 -0.7203622272 -1.1194251  1.0443674      A
```
