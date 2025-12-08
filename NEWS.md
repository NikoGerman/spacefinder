# spacefinder (development version)

## Bug Fixes

* Fixed issues in `coef()` method for Box Learner when only one hyperparameter is present

# spacefinder 0.2.1

## Internal changes

* Refactored to use `generics::augment()` instead of custom generic
* `augment()` methods now properly dispatch through generics package

# spacefinder 0.2.0

## Major Changes

* Comprehensive roxygen2 documentation for all exported functions and classes
* Added four vignettes: getting-started, learner-comparison, density-estimation, categorical-hyperparameters
* Added `benchmark_data` dataset for examples and vignettes
* Standardized transformation convention: `y = Ax + b` maps from fitted subspace to unit hypercube

## New Features

* `augment()` methods for Box and Polygon learners to fit Beta densities
* `coef.LearnerSubspaceBox(vectorize = TRUE)` now returns transformation matrices A, b

## Breaking Changes

* `LearnerSubspaceElips` is now called `LearnerSubspaceEllipsoid`
* `LearnerSubspaceBoxGeneral` is now called `LearnerSubspacePolygon`

## Bug Fixes

* Fixed data.table NSE issues in `coef()` methods
* Fixed formula parsing in TaskSubspace initialization
* Corrected transformation in `augment.LearnerSubspacePolygon()` 
* Fixed roxygen2 cross-reference warnings

## Documentation

* Complete documentation for all R6 classes (LearnerSubspace, TaskSubspace, and subclasses)
* Documented all S3 methods (coef, augment, autoplot, summary)
* Added mathematical notation and detailed algorithm descriptions
* Cross-referenced related functions throughout

# spacefinder 0.1.0

* Initial release
* Implemented LearnerSubspaceBox, LearnerSubspaceElips, LearnerSubspaceOrientedBox
* Added methods: coef(), summary(), autoplot(), outliers()

## Features
* `coef()` methods for all three learner types
* `autoplot()` methods for all three learner types
* `summary()` method for comprehensive learner overview
* `outliers()` function to extract outlier configurations
* `as_task_subspace()` convenience function

