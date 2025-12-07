# spacefinder: Subspace Learning for Hyperparameter Optimization

Learn promising hyperparameter subspaces from benchmark data using
geometric methods. The package provides learners for fitting
axis-aligned hyperrectangles, oriented hyperrectangles, and ellipsoids
to high-performing hyperparameter configurations.

## Details

**Main Components:**

- [`TaskSubspace`](https://nikogerman.github.io/spacefinder/reference/TaskSubspace.md):
  Define hyperparameter optimization tasks

- [`LearnerSubspaceBox`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceBox.md):
  Fit axis-aligned hyperrectangles

- [`LearnerSubspacePolygon`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspacePolygon.md):
  Fit oriented hyperrectangles

- [`LearnerSubspaceEllipsoid`](https://nikogerman.github.io/spacefinder/reference/LearnerSubspaceEllipsoid.md):
  Fit ellipsoids

**Key Methods:**

- `train()`: Fit subspace to top-performing configurations

- [`coef()`](https://rdrr.io/r/stats/coef.html): Extract fitted subspace
  parameters

- `augment()`: Add beta density parameters

- `autoplot()`: Visualize fitted subspaces

- [`outliers()`](https://nikogerman.github.io/spacefinder/reference/outliers.md):
  Extract outlier configurations

## See also

Useful links:

- <https://nikogerman.github.io/spacefinder/>

## Author

**Maintainer**: Nikolai German <niko.german@gmail.com>
([ORCID](https://orcid.org/0009-0001-7394-8367))
