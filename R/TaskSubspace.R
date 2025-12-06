#' @title Subspace task definition
#' @description
#' R6 class for defining hyperparameter optimization tasks. Encapsulates
#' benchmark data with hyperparameter configurations and performance measures,
#' providing a standardized interface for subspace learners.
#'
#' @details
#' \strong{Overview:}
#'
#' A subspace task combines:
#' \itemize{
#'   \item Benchmark data with hyperparameter configurations and performance measures
#'   \item Specification of which columns are hyperparameters (continuous and categorical)
#'   \item Target performance measure to optimize
#' }
#'
#' \strong{Data Requirements:}
#'
#' The input \code{data.table} must contain:
#' \itemize{
#'   \item A \code{task} column identifying different tasks/datasets
#'   \item A target measure column (e.g., "auc", "accuracy", "rmse")
#'   \item One or more numeric hyperparameter columns
#'   \item Optionally, one categorical hyperparameter column
#' }
#'
#' \strong{Initialization Modes:}
#'
#' \strong{Mode 1: Explicit specification}
#' \preformatted{
#' task <- TaskSubspace$new(
#'   data = benchmark_data,
#'   target_measure = "accuracy",
#'   hps = c("learning_rate", "max_depth"),
#'   cat_hps = "optimizer"
#' )
#' }
#'
#' \strong{Mode 2: Formula interface}
#' \preformatted{
#' task <- TaskSubspace$new(
#'   data = benchmark_data,
#'   formula = accuracy ~ (learning_rate + max_depth) * optimizer
#' )
#' }
#'
#' The formula syntax is: \code{target ~ (hp1 + hp2 + ...) * cat_hp}
#' \itemize{
#'   \item Left-hand side: target measure
#'   \item Right-hand side before \code{*}: continuous hyperparameters
#'   \item Right-hand side after \code{*}: categorical hyperparameter (optional)
#' }
#'
#' \strong{Categorical Hyperparameters:}
#'
#' Currently supports at most one categorical hyperparameter. When specified,
#' learners will fit separate subspaces for each categorical level.
#'
#' @field data A \code{data.table} containing hyperparameter configurations,
#'   performance measures, and task identifiers. Must have a \code{task} column.
#' @field target_measure Character string specifying the performance measure
#'   column name (e.g., "auc", "accuracy", "rmse")
#' @field hps Character vector of continuous hyperparameter column names
#' @field cat_hps Character string specifying the categorical hyperparameter
#'   column name (optional, currently limited to one)
#'
#' @seealso
#' \code{\link{LearnerSubspace}} for the base learner class.
#' \code{\link{LearnerSubspaceBox}} for axis-aligned hyperrectangles.
#' \code{\link{LearnerSubspacePolygon}} for oriented hyperrectangles.
#' \code{\link{LearnerSubspaceEllipsoid}} for ellipsoids.
#'
#' @examples
#' \dontrun{
#' # Explicit specification
#' task <- TaskSubspace$new(
#'   data = benchmark_data,
#'   target_measure = "auc",
#'   hps = c("learning_rate", "max_depth", "min_samples_split")
#' )
#'
#' # With categorical hyperparameter
#' task <- TaskSubspace$new(
#'   data = benchmark_data,
#'   target_measure = "accuracy",
#'   hps = c("learning_rate", "max_depth"),
#'   cat_hps = "optimizer"
#' )
#'
#' # Formula interface
#' task <- TaskSubspace$new(
#'   data = benchmark_data,
#'   formula = auc ~ (learning_rate + max_depth) * optimizer
#' )
#'
#' # Use with learner
#' learner <- LearnerSubspaceBox$new(task)
#' learner$train(q_val = 0.9)
#' }
#'
#' @export
TaskSubspace <- R6::R6Class(
  "TaskSubspace",
  public = list(
    data = NULL,
    target_measure = NULL,
    hps = NULL,
    cat_hps = NULL,

    #' @description Create a new task instance
    #' @param data A data.table containing task performance data
    #' @param formula Formula specification (optional)
    #' @param target_measure Name of the performance measure column
    #' @param hps Character vector of continuous hyperparameter names
    #' @param cat_hps Character vector of categorical hyperparameter names (optional)
    initialize = function(
      data,
      formula = NULL,
      target_measure = NULL,
      hps = NULL,
      cat_hps = NULL
    ) {
      checkmate::assertDataTable(data)

      if (!"task" %in% colnames(data)) {
        stop("Data must contain a 'task' column", call. = FALSE)
      }

      if (is.null(formula)) {
        if (is.null(target_measure) || is.null(hps)) {
          stop(
            "Must provide either 'formula', or both 'target_measure' and 'hps'",
            call. = FALSE
          )
        }

        checkmate::assertCharacter(
          hps,
          min.len = 1,
          null.ok = FALSE,
          any.missing = FALSE
        )
        checkmate::assertCharacter(
          cat_hps,
          max.len = 1,
          null.ok = TRUE,
          any.missing = FALSE
        )
        checkmate::assertCharacter(
          target_measure,
          len = 1,
          null.ok = FALSE,
          any.missing = FALSE
        )

        if (!target_measure %in% colnames(data)) {
          stop(
            "'target_measure' column not found in data: ",
            target_measure,
            call. = FALSE
          )
        }

        missing_hps <- setdiff(hps, colnames(data))
        if (length(missing_hps) > 0) {
          stop(
            "Hyperparameter columns not found in data: ",
            paste(missing_hps, collapse = ", "),
            call. = FALSE
          )
        }

        if (!is.null(cat_hps) && !cat_hps %in% colnames(data)) {
          stop("'cat_hps' column not found in data: ", cat_hps, call. = FALSE)
        }

        self$hps <- hps
        self$cat_hps <- cat_hps
        self$target_measure <- target_measure
      } else {
        if (!is.null(target_measure) || !is.null(hps) || !is.null(cat_hps)) {
          warning(
            "When 'formula' is provided, 'target_measure', 'hps', and 'cat_hps' are ignored",
            call. = FALSE
          )
        }

        form <- tryCatch(
          as.formula(formula),
          error = function(e) {
            stop("Invalid formula specification: ", e$message, call. = FALSE)
          }
        )

        target_vars <- all.vars(form[[2]])
        if (length(target_vars) != 1) {
          stop(
            "Formula left-hand side must specify exactly one target measure",
            call. = FALSE
          )
        }

        if (!target_vars %in% colnames(data)) {
          stop(
            "Target measure column not found in data: ",
            target_vars,
            call. = FALSE
          )
        }

        self$target_measure <- target_vars

        rhs <- form[[3]]

        if (length(rhs) == 3 && as.character(rhs[[1]]) == "*") {
          self$hps <- all.vars(rhs[[2]])
          self$cat_hps <- all.vars(rhs[[3]])

          if (length(self$cat_hps) > 1) {
            stop(
              "Currently only one categorical hyperparameter is supported",
              call. = FALSE
            )
          }
        } else {
          self$hps <- all.vars(rhs)
          self$cat_hps <- NULL
        }

        missing_hps <- setdiff(self$hps, colnames(data))
        if (length(missing_hps) > 0) {
          stop(
            "Hyperparameter columns not found in data: ",
            paste(missing_hps, collapse = ", "),
            call. = FALSE
          )
        }

        if (!is.null(self$cat_hps) && !self$cat_hps %in% colnames(data)) {
          stop(
            "Categorical hyperparameter column not found in data: ",
            self$cat_hps,
            call. = FALSE
          )
        }
      }

      self$data <- data
    }
  )
)
