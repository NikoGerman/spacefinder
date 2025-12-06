#' @include LearnerSubspaceBox.R
#' @title Extract coefficients from fitted axis-aligned box learner
#' @description
#' Extracts fitted subspace parameters from a trained \code{LearnerSubspaceBox}
#' object. Returns either explicit hyperparameter bounds or the transformation
#' matrices that map the fitted axis-aligned hyperrectangle to the unit hypercube.
#'
#' @param object A \code{LearnerSubspaceBox} object with fitted subspace parameters
#' @param vectorize `logical` whether to return transformation matrices (\code{TRUE})
#'   or explicit bounds (\code{FALSE}). Default: \code{FALSE}
#'   \itemize{
#'     \item \code{FALSE}: Returns \code{data.table} with columns \code{hyperparameter},
#'       \code{min}, \code{max} for each hyperparameter
#'     \item \code{TRUE}: Returns \code{data.table} with columns \code{hyperparameters}
#'       (list), \code{A} (diagonal matrix), \code{b} (translation vector)
#'   }
#' @param ... Additional arguments (currently unused)
#'
#' @return A \code{data.table} containing fitted subspace parameters.
#'
#'   \strong{When \code{vectorize = FALSE} (explicit bounds):}
#'   \itemize{
#'     \item \code{hyperparameter}: Hyperparameter name
#'     \item \code{min}: Lower bound of fitted interval
#'     \item \code{max}: Upper bound of fitted interval
#'     \item \code{cat_hp}: Categorical level (only if task has categorical hyperparameters)
#'   }
#'
#'   \strong{When \code{vectorize = TRUE} (transformation matrices):}
#'   \itemize{
#'     \item \code{hyperparameters}: List column containing hyperparameter names
#'     \item \code{A}: List column of diagonal matrices with \eqn{1/(max - min)} on diagonal
#'     \item \code{b}: List column of translation vectors equal to \eqn{-min/(max - min)}
#'     \item \code{cat_hp}: Categorical level (only if task has categorical hyperparameters)
#'   }
#'
#' @details
#' For axis-aligned hyperrectangles, the transformation from the fitted subspace
#' to the unit hypercube \eqn{[0,1]^d} is:
#' \deqn{y = Ax + b}
#' where:
#' \itemize{
#'   \item \eqn{A = diag(1/(max - min))} is a diagonal matrix (independent scaling per dimension)
#'   \item \eqn{b = -min/(max - min)} is the translation vector
#'   \item \eqn{x \in [min, max]^d} are original hyperparameter coordinates
#'   \item \eqn{y \in [0,1]^d} are unit cube coordinates
#' }
#'
#' This maps each hyperparameter from its fitted range \eqn{[min, max]} to \eqn{[0, 1]}.
#'
#' When the task includes categorical hyperparameters, separate coefficient sets
#' are returned for each categorical level, identified by the \code{cat_hp} column.
#'
#' @section Error Handling:
#' Throws an error if the learner has not been trained. Call \code{train()} before
#' extracting coefficients.
#'
#' @seealso
#' \code{\link{LearnerSubspaceBox}} for learner class.
#' \code{\link{coef.LearnerSubspacePolygon}} for the oriented hyperrectangle variant.
#' \code{\link{augment.LearnerSubspaceBox}} for adding density parameters.
#'
#' @examples
#' \dontrun{
#' # Train a box learner
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train()
#'
#' # Get explicit bounds
#' coef(learner)
#'
#' # Get transformation matrices
#' coef(learner, vectorize = TRUE)
#' }
#'
#' @exportS3Method
coef.LearnerSubspaceBox <- function(
  object,
  vectorize = FALSE,
  ...
) {
  checkmate::assertFlag(vectorize)
  if (is.null(object$result)) {
    stop(
      "Learner has not been trained. Use train() method first.",
      call. = FALSE
    )
  }
  if (!is.null(object$task$cat_hps)) {
    cat_hp <- object$task$cat_hps
    coefs <- data.table::rbindlist(
      lapply(object$result, \(x) x$coefficients),
      idcol = cat_hp
    )
    if (vectorize) {
      return(
        coefs[,
          list(
            hyperparameters = list(hyperparameter),
            A = list(diag(1 / (max - min))),
            b = list(-min / (max - min))
          ),
          by = cat_hp
        ]
      )
    } else {
      return(coefs)
    }
  } else {
    coefs <- object$result$coefficients
    if (vectorize) {
      return(
        coefs[, list(
          hyperparameters = list(hyperparameter),
          A = list(diag(1 / (max - min))),
          b = list(-min / (max - min))
        )]
      )
    } else {
      return(coefs)
    }
  }
}
