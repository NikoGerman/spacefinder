# Changelog

## spacefinder (development version)

### Bug Fixes

- Fixed issues in [`coef()`](https://rdrr.io/r/stats/coef.html) method
  for Box Learner when only one hyperparameter is present
- Fixed issue in [`coef()`](https://rdrr.io/r/stats/coef.html) method
  for Box Learner with setting `vectorize = TRUE` and degenerate case,
  where min=max.
- Fixed faulty normalization of weights within
  [`augment()`](https://generics.r-lib.org/reference/augment.html).

## spacefinder 0.2.1

### Internal changes

- Refactored to use
  [`generics::augment()`](https://generics.r-lib.org/reference/augment.html)
  instead of custom generic
- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods now properly dispatch through generics package

## spacefinder 0.2.0

### Major Changes

- Comprehensive roxygen2 documentation for all exported functions and
  classes
- Added four vignettes: getting-started, learner-comparison,
  density-estimation, categorical-hyperparameters
- Added `benchmark_data` dataset for examples and vignettes
- Standardized transformation convention: `y = Ax + b` maps from fitted
  subspace to unit hypercube

### New Features

- [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods for Box and Polygon learners to fit Beta densities
- `coef.LearnerSubspaceBox(vectorize = TRUE)` now returns transformation
  matrices A, b

### Breaking Changes

- `LearnerSubspaceElips` is now called `LearnerSubspaceEllipsoid`
- `LearnerSubspaceBoxGeneral` is now called `LearnerSubspacePolygon`

### Bug Fixes

- Fixed data.table NSE issues in
  [`coef()`](https://rdrr.io/r/stats/coef.html) methods
- Fixed formula parsing in TaskSubspace initialization
- Corrected transformation in
  [`augment.LearnerSubspacePolygon()`](https://nikogerman.github.io/spacefinder/reference/augment.LearnerSubspacePolygon.md)
- Fixed roxygen2 cross-reference warnings

### Documentation

- Complete documentation for all R6 classes (LearnerSubspace,
  TaskSubspace, and subclasses)
- Documented all S3 methods (coef, augment, autoplot, summary)
- Added mathematical notation and detailed algorithm descriptions
- Cross-referenced related functions throughout

## spacefinder 0.1.0

- Initial release
- Implemented LearnerSubspaceBox, LearnerSubspaceElips,
  LearnerSubspaceOrientedBox
- Added methods: coef(), summary(), autoplot(), outliers()

### Features

- [`coef()`](https://rdrr.io/r/stats/coef.html) methods for all three
  learner types
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods for all three learner types
- [`summary()`](https://rdrr.io/r/base/summary.html) method for
  comprehensive learner overview
- [`outliers()`](https://nikogerman.github.io/spacefinder/reference/outliers.md)
  function to extract outlier configurations
- [`as_task_subspace()`](https://nikogerman.github.io/spacefinder/reference/as_task_subspace.md)
  convenience function
