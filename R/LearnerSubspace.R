#' @title LearnerSubspace base class
#' @export
LearnerSubspace <- R6::R6Class(
  "LearnerSubspace",
  #' @field task A TaskSubspace object
  #' @field result training result
  #' @field top_configs the best hyperparameter configurations w.r.t. to q-value and target_measure
  public = list(
    task = NULL,
    result = NULL,
    top_configs = NULL,

    #' @param task A `TaskSubspace` object
    initialize = function(task) {
      stopifnot(inherits(task, "TaskSubspace"))
      self$task <- task
    },

    #' @param tasks Character vector of task names to include (optional, yet mutually exclusive with exclude_tasks)
    #' @param exclude_tasks Character vector of task names to exclude (optional)
    #' @param q_val Quantile threshold for filtering configurations (0-1)
    #' @param lambda Regularization parameter for slack variables (default = NULL, no slack)
    #'               Higher values allow more points outside the subspace
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
        FUN = private$.fit_subspace, # Abstract method implemented by subclasses
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

    # Validate common training inputs
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

      # Validate task names exist in data
      if (!is.null(tasks) && !all(tasks %in% self$task$data$task)) {
        stop("All provided 'tasks' must be present in data")
      }
      if (
        !is.null(exclude_tasks) && !all(exclude_tasks %in% self$task$data$task)
      ) {
        stop("All provided 'exclude_tasks' must be present in data")
      }
    },

    # Resolve which tasks to use based on tasks/exclude_tasks arguments
    .resolve_tasks = function(tasks, exclude_tasks) {
      if (!is.null(tasks)) {
        return(tasks)
      }
      # exclude_tasks is provided
      return(setdiff(unique(self$task$data$task), exclude_tasks))
    },

    # Filter data to top quantile configurations
    .filter_top_quantile = function(
      data,
      target_measure,
      hps,
      cat_hps,
      q_val = 1
    ) {
      # Determine columns to keep and grouping variables
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

    #' Agnostically compute subspaces
    .compute_subspaces = function(
      data,
      FUN,
      lambda,
      hps,
      cat_hps
    ) {
      if (!is.null(cat_hps)) {
        # Handle categorical hyperparameters - separate fit per category
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
