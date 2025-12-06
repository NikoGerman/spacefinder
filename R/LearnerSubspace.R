#' @title Subspace learner base class
#' @description
#' Abstract R6 base class for learning hyperparameter subspaces that contain
#' high-quality configurations. Provides a unified framework for fitting
#' geometric regions (hyperrectangles, ellipsoids) to top-performing
#' hyperparameter configurations identified by a quantile threshold.
#'
#' @details
#' \strong{Overview:}
#'
#' Subspace learners identify promising regions in hyperparameter space by:
#' \enumerate{
#'   \item Filtering configurations to top quantile based on performance measure
#'   \item Fitting a geometric subspace (implementation-specific) to filtered data
#'   \item Optionally allowing outliers via regularization parameter \code{lambda}
#' }
#'
#' \strong{Geometric Representations:}
#'
#' All learners represent subspaces via transformation \eqn{y = Ax + b} where:
#' \itemize{
#'   \item \eqn{x \in [0,1]^p} are unit cube coordinates
#'   \item \eqn{y \in \mathbb{R}^p} are original hyperparameter coordinates
#'   \item \eqn{A \in \mathbb{R}^{p \times p}} defines shape and orientation
#'   \item \eqn{b \in \mathbb{R}^p} is the translation vector
#' }
#'
#' Different learner types impose different structure on matrix \eqn{A}:
#' \itemize{
#'   \item \strong{Box}: \eqn{A} is diagonal (axis-aligned hyperrectangle)
#'   \item \strong{Polygon}: \eqn{A} is general positive definite (oriented hyperrectangle)
#'   \item \strong{Ellipsoid}: \eqn{A} is general positive definite (full ellipsoid)
#' }
#'
#' \strong{Categorical Hyperparameters:}
#'
#' When the task includes categorical hyperparameters, separate subspaces are
#' fitted for each categorical level independently. This allows different
#' geometries for different categories (e.g., different learning rate ranges
#' per optimizer).
#'
#' \strong{Regularization via Slack Variables:}
#'
#' The \code{lambda} parameter controls the volume-outlier trade-off:
#' \itemize{
#'   \item \code{lambda = NULL}: Hard constraints, all points must fit inside
#'   \item \code{lambda > 0}: Soft constraints, allows outliers with penalty
#'   \item Larger \code{lambda}: Smaller subspaces, more outliers tolerated
#'   \item Smaller \code{lambda}: Larger subspaces, fewer outliers tolerated
#' }
#'
#' \strong{Workflow:}
#'
#' \preformatted{
#' # 1. Create task
#' task <- SubspaceTask$new(data, target_measure = "accuracy")
#'
#' # 2. Initialize learner (use specific subclass)
#' learner <- LearnerSubspaceBox$new(task)
#'
#' # 3. Train on top configurations
#' learner$train(q_val = 0.9, lambda = 0.1)
#'
#' # 4. Extract fitted parameters
#' coef(learner, vectorize = TRUE)
#'
#' # 5. Add density parameters
#' augment(learner)
#' }
#'
#' @seealso
#' \code{\link{LearnerSubspaceBox}} for axis-aligned hyperrectangles.
#' \code{\link{LearnerSubspacePolygon}} for oriented hyperrectangles.
#' \code{\link{LearnerSubspaceEllipsoid}} for ellipsoids.
#' \code{\link{TaskSubspace}} for task definition.
#'
#' @examples
#' \dontrun{
#' # This is an abstract class - use specific implementations
#'
#' # Create task
#' task <- SubspaceTask$new(
#'   data = benchmark_data,
#'   target_measure = "auc",
#'   cat_hps = "optimizer"
#' )
#'
#' # Use Box learner (axis-aligned)
#' learner_box <- LearnerSubspaceBox$new(task)
#' learner_box$train(q_val = 0.9, lambda = 0.1)
#'
#' # Use Ellipsoid learner (most flexible)
#' learner_ellip <- LearnerSubspaceEllipsoid$new(task)
#' learner_ellip$train(q_val = 0.95, lambda = NULL)
#'
#' # Filter specific tasks
#' learner_box$train(
#'   q_val = 0.8,
#'   tasks = c("task1", "task2"),
#'   lambda = 0.05
#' )
#' }
#' @export
LearnerSubspace <- R6::R6Class(
  "LearnerSubspace",
  public = list(
    #' @field task A TaskSubspace object
    task = NULL,
    #' @field result Training result
    result = NULL,
    #' @field top_configs Top hyperparameter configurations after quantile filtering
    top_configs = NULL,

    #' @description Create a new learner instance
    #' @param task A \code{TaskSubspace} object
    initialize = function(task) {
      stopifnot(inherits(task, "TaskSubspace"))
      self$task <- task
    },

    #' @description Train the learner on top-quantile configurations
    #' @param q_val Quantile threshold for filtering configurations (0-1)
    #' @param lambda Regularization parameter for slack variables (default = NULL)
    #' @param tasks Character vector of task names to include (optional)
    #' @param exclude_tasks Character vector of task names to exclude (optional)
    train = function(
      q_val = 1,
      lambda = NULL,
      tasks = NULL,
      exclude_tasks = NULL
    ) {
      if (is.null(tasks) & is.null(exclude_tasks)) {
        tasks <- unique(self$task$data$task)
      }

      # Validate inputs
      private$.validate_train_inputs(
        tasks = tasks,
        exclude_tasks = exclude_tasks,
        q_val = q_val,
        lambda = lambda
      )

      # Resolve which tasks to use
      selected_tasks <- private$.resolve_tasks(tasks, exclude_tasks)

      # Filter data to selected tasks
      selected_data <- self$task$data[task %in% selected_tasks]

      # Store top configurations for each task
      self$top_configs <- private$.filter_top_quantile(
        selected_data,
        target_measure = self$task$target_measure,
        hps = self$task$hps,
        cat_hps = self$task$cat_hps,
        q_val = q_val
      )

      # Compute subspaces using subclass-specific fitting method
      self$result <- private$.compute_subspaces(
        data = self$top_configs,
        FUN = private$.fit_subspace,
        hps = self$task$hps,
        cat_hps = self$task$cat_hps,
        lambda = lambda
      )

      invisible(self)
    }
  ),

  private = list(
    .fit_subspace = function(data, lambda, ...) {
      stop("Subclasses must implement the .fit_subspace() method")
    },

    .validate_train_inputs = function(
      tasks,
      exclude_tasks,
      q_val,
      lambda
    ) {
      checkmate::assertCharacter(
        tasks,
        min.len = 1,
        null.ok = TRUE,
        any.missing = FALSE
      )
      checkmate::assertCharacter(
        exclude_tasks,
        min.len = 1,
        null.ok = TRUE,
        any.missing = FALSE
      )
      checkmate::assertNumeric(
        q_val,
        lower = 0,
        upper = 1,
        len = 1,
        any.missing = FALSE
      )
      checkmate::assertNumeric(
        lambda,
        lower = 2e-15,
        len = 1,
        null.ok = TRUE,
        any.missing = FALSE
      )

      if (!is.null(tasks) && !all(tasks %in% self$task$data$task)) {
        stop("All provided 'tasks' must be present in data")
      }
      if (
        !is.null(exclude_tasks) && !all(exclude_tasks %in% self$task$data$task)
      ) {
        stop("All provided 'exclude_tasks' must be present in data")
      }
    },

    .resolve_tasks = function(tasks, exclude_tasks) {
      if (!is.null(tasks)) {
        return(tasks)
      }
      return(base::setdiff(unique(self$task$data$task), exclude_tasks))
    },

    .filter_top_quantile = function(
      data,
      target_measure,
      hps,
      cat_hps,
      q_val = 1
    ) {
      keep_cols <- c("task", target_measure, hps)
      by_vars <- "task"

      if (!is.null(cat_hps)) {
        keep_cols <- c(cat_hps, keep_cols)
        by_vars <- c(by_vars, cat_hps)
      }

      data_with_qantile <- data[,
        q := quantile(get(target_measure), q_val),
        by = by_vars
      ]

      return(data_with_qantile[get(target_measure) >= q, -"q", ])
    },

    .compute_subspaces = function(
      data,
      FUN,
      lambda,
      hps,
      cat_hps
    ) {
      if (!is.null(cat_hps)) {
        result <- list()
        levels <- unique(data[[cat_hps]])

        for (level in levels) {
          actual_hps <- names(which(data[
            get(cat_hps) == level,
            vapply(mget(hps), \(x) all(!is.na(x)), logical(1))
          ]))
          level_data <- data[get(cat_hps) == level, mget(actual_hps)]

          result[[as.character(level)]] <- FUN(
            data = level_data,
            lambda = lambda
          )
        }
        return(result)
      } else {
        hp_data <- data[, mget(hps)]
        return(FUN(data = hp_data, lambda = lambda))
      }
    }
  )
)
