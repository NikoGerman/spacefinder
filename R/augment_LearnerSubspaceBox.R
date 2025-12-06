#' @include LearnerSubspaceBox.R helper.R
#' @title Augment axis-aligned box learner with beta densities
#' @description
#' Transforms data from the fitted axis-aligned hyperrectangle subspace to the unit
#' hypercube and fits univariate beta distributions to each hyperparameter dimension
#' using weighted maximum likelihood estimation. Since box learners use diagonal
#' transformation matrices (independent scaling), inversion is computationally efficient.
#'
#' @param object A \code{LearnerSubspaceBox} object with fitted subspace parameters
#' @param regularize Logical indicating whether to enforce \code{alpha >= 1} and
#'   \code{beta >= 1} to avoid U-shaped densities. Default: \code{TRUE}
#' @param ... Additional arguments passed to \code{\link{fit_beta_mle_single}}
#'   (e.g., \code{tol}, \code{max_iter}, \code{clip_eps})
#'
#' @return A \code{data.table} with columns:
#'   \itemize{
#'     \item \code{parameter}: Hyperparameter name
#'     \item \code{alpha}: Fitted beta shape parameter (alpha > 0)
#'     \item \code{beta}: Fitted beta shape parameter (beta > 0)
#'     \item \code{converged}: Logical indicating whether MLE converged
#'     \item \code{iterations}: Number of Newton-Raphson iterations used
#'     \item \code{cat_hp}: Categorical level (only if task has categorical hyperparameters)
#'   }
#'
#' @details
#' \strong{Algorithm:}
#'
#' For each categorical level (or globally if no categorical hyperparameters):
#' \enumerate{
#'   \item Extract fitted box bounds (min, max) for each hyperparameter
#'   \item Transform data to unit cube: \eqn{x = (y - min) / (max - min)}
#'   \item Filter points where all coordinates lie in \eqn{[0,1]^d}
#'   \item Fit \eqn{Beta(\alpha, \beta)} to each dimension via weighted MLE
#'   \item Apply regularization if requested: \eqn{\alpha, \beta \geq 1}
#' }
#'
#' If no valid points remain after filtering (shouldn't happen for box learners with
#' lambda = NULL), returns uniform prior \eqn{Beta(1,1)} for all dimensions.
#'
#' \strong{Weights:}
#'
#' Data points are weighted by their performance (target measure values), normalized
#' to sum to 1. This gives more influence to higher-performing configurations when
#' fitting the beta distributions.
#'
#' \strong{Regularization:}
#'
#' When \code{regularize = TRUE} (default), ensures \eqn{\alpha \geq 1} and
#' \eqn{\beta \geq 1}. This prevents U-shaped densities (which occur when both
#' parameters are less than 1) and ensures the mode exists in the interior of \eqn{[0,1]}.
#'
#' \strong{Categorical Hyperparameters:}
#'
#' When the task includes categorical hyperparameters, separate beta distributions
#' are fitted for each combination of hyperparameter and categorical level.
#'
#' @seealso
#' \code{\link{LearnerSubspaceBox}} for the learner class.
#' \code{\link{fit_beta_mle_single}} for the univariate beta MLE algorithm.
#' \code{\link{augment.LearnerSubspacePolygon}} for the oriented hyperrectangle variant.
#'
#' @examples
#' \dontrun{
#' # Train learner
#' task <- TaskSubspace$new(data, target_measure = "auc",
#'                          hps = c("learning_rate", "max_depth"))
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.9)
#'
#' # Fit beta densities
#' densities <- augment(learner)
#' print(densities)
#'
#' # Without regularization (allows U-shaped densities)
#' densities_unreg <- augment(learner, regularize = FALSE)
#'
#' # Sample from fitted distributions
#' n_samples <- 100
#' sampled_lr <- rbeta(n_samples,
#'                     densities[parameter == "learning_rate"]$alpha,
#'                     densities[parameter == "learning_rate"]$beta)
#' }
#'
#' @exportS3Method
augment.LearnerSubspaceBox <- function(object, regularize = TRUE, ...) {
  if (is.null(object$result)) {
    stop(
      "Learner has not been trained. Use train() method first.",
      call. = FALSE
    )
  }

  data <- object$task$data
  coefs <- coef(object, vectorize = TRUE)
  cat_hps <- object$task$cat_hps

  # Helper function to fit beta densities for one level
  fit_level <- function(level_data, hps, A, b, w) {
    # Normalize weights
    w <- w / sum(w)

    # Transform to [-1, 0]^d: z = Ax + b
    y <- as.matrix(level_data[, mget(hps)])
    x <- sweep(t(A %*% t(y)), 2, b, FUN = `+`)

    # Create data.table with transformed coordinates
    DT <- data.table::as.data.table(x)
    data.table::setnames(DT, hps)
    DT[, w := w]

    # Filter valid points (within [0,1]^d)
    DT[, .keep := rowSums(.SD < 0 | .SD > 1) == 0, .SDcols = hps]
    DT <- DT[.keep == TRUE][, .keep := NULL]

    # Handle case with no valid points
    if (nrow(DT) == 0) {
      warning(
        "No valid points in unit cube, returning uniform prior Beta(1,1)",
        call. = FALSE
      )
      return(data.table::data.table(
        parameter = hps,
        alpha = 1.0,
        beta = 1.0,
        converged = TRUE,
        iterations = 0L
      ))
    }

    # Fit beta MLE for each hyperparameter
    data.table::rbindlist(lapply(hps, function(hp) {
      fit <- fit_beta_mle_single(DT[[hp]], DT$w, ...)

      # Apply regularization
      if (regularize) {
        fit$alpha <- max(1.0, fit$alpha)
        fit$beta <- max(1.0, fit$beta)
      }

      data.table::data.table(
        parameter = hp,
        alpha = fit$alpha,
        beta = fit$beta,
        converged = fit$converged,
        iterations = fit$iterations
      )
    }))
  }

  # Process by categorical level if present
  if (!is.null(cat_hps)) {
    data.table::rbindlist(lapply(unique(coefs[, get(cat_hps)]), function(lvl) {
      coef_row <- coefs[get(cat_hps) == lvl]
      fit_level(
        level_data = data[get(cat_hps) == lvl],
        hps = coef_row$hyperparameters[[1]],
        A = coef_row$A[[1]],
        b = coef_row$b[[1]],
        w = data[get(cat_hps) == lvl, get(object$task$target_measure)]
      )[, (cat_hps) := lvl]
    }))
  } else {
    fit_level(
      level_data = data,
      hps = coefs$hyperparameters[[1]],
      A = coefs$A[[1]],
      b = coefs$b[[1]],
      w = data[[object$task$target_measure]]
    )
  }
}
