# Check for required package dependencies

Verifies that required packages are installed and can be loaded. Throws
an error with a helpful message listing any missing packages.

## Usage

``` r
check_packages(pkgs)
```

## Arguments

- pkgs:

  `character` vector of package names to check

## Value

Invisible `NULL` if all packages are available. Otherwise throws an
error.

## Details

Uses [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) to
check package availability without loading them. This is preferred over
[`require()`](https://rdrr.io/r/base/library.html) for package
dependency checks as it doesn't attach packages to the search path.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check single package
check_packages("data.table")

# Check multiple packages
check_packages(c("ggplot2", "dplyr"))
} # }
```
