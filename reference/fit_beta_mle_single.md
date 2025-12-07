# Fit univariate beta distribution via weighted MLE

Fits a Beta(alpha, beta) distribution to univariate data using weighted
maximum likelihood estimation. Initializes parameters via weighted
moment matching, then refines estimates using Newton-Raphson
optimization.

## Usage

``` r
fit_beta_mle_single(x, w, tol = 1e-06, max_iter = 100, clip_eps = NULL)
```

## Arguments

- x:

  `numeric` vector of observations in the interval (0, 1)

- w:

  `numeric` vector of normalized weights that sum to 1. Must have the
  same length as `x`

- tol:

  `numeric` convergence tolerance for gradient norm (default: 1e-6)

- max_iter:

  `integer` maximum number of Newton-Raphson iterations (default: 100)

- clip_eps:

  `numeric` clipping epsilon for numerical stability. Values outside
  `[clip_eps, 1 - clip_eps]` are clipped. Default: same as `tol`

## Value

A `list` with components:

- alpha:

  fitted shape parameter (\> 0)

- beta:

  fitted shape parameter (\> 0)

- converged:

  logical indicating whether optimization converged

- iterations:

  number of Newton-Raphson iterations performed

## Details

The function optimizes the weighted log-likelihood: \$\$\sum_i w_i \log
f(x_i; \alpha, \beta)\$\$ where \\f(x; \alpha, \beta)\\ is the beta
density.

**Algorithm:**

1.  Clip `x` to `[clip_eps, 1 - clip_eps]` for numerical stability

2.  Initialize via weighted method of moments

3.  Iteratively update parameters using Newton-Raphson with backtracking
    line search

4.  Convergence declared when gradient norm \< `tol`

**Numerical considerations:**

- Returns alpha = beta = 100 for degenerate cases (variance \< 1e-10)

- Uses backtracking line search to maintain positivity of parameters

- Warns if Hessian becomes singular (determinant \< 1e-10)

- Warns if maximum iterations reached without convergence

## Examples

``` r
if (FALSE) { # \dontrun{
# Unweighted fit
x <- rbeta(100, 2, 5)
w <- rep(1/100, 100)
fit <- fit_beta_mle_single(x, w)

# Weighted fit
x <- rbeta(100, 3, 3)
w <- runif(100)
w <- w / sum(w)
fit <- fit_beta_mle_single(x, w, tol = 1e-8, max_iter = 200)
} # }
```
