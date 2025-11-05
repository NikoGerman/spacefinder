#' @include LearnerSubspaceBox.R LearnerSubspaceElips.R
#' @title summary method for LearnerSubspace
#' @param object a object which inherits from LearnerSubspace
#' @param ... Additional arguments (unused)
#' @export
summary.LearnerSubspace <- function(
  object,
  ...
) {
  if (is.null(object$result)) {
    stop("Use 'train()' method first")
  }

  if (!is.null(object$task$cat_hps)) {
    levels <- names(object$result)
    observations <- list()
    for (level in levels) {
      observations[[length(observations) + 1]] <- nrow(object$top_configs[
        get(object$task$cat_hps) == level,
      ])
    }
    status <- lapply(object$result, \(x) x$status)
    objective_value <- lapply(object$result, \(x) x$objective_value)
    n_violations <- lapply(object$result, \(x) x$n_violations)
    dt <- data.table::data.table(
      names(object$result),
      status,
      objective_value,
      n_violations,
      observations
    )
    data.table::setnames(
      dt,
      c(
        object$task$cat_hps,
        "status",
        "objective_value",
        "n_violations",
        "observations"
      )
    )
  } else {
    dt <- data.table::data.table(
      status = object$result$status,
      objective_value = object$result$objective_value,
      n_violations = object$result$n_violations,
      observations = nrow(object$top_configs)
    )
  }
  coefs <- stats::coef(object)
  outlier <- suppressMessages(outliers(object))

  # Create a summary info table
  info_dt <- data.table::data.table(
    Property = c(
      "Target Measure",
      "Numeric Hyperparameters",
      "Categorical Hyperparameters"
    ),
    Value = c(
      object$task$target_measure,
      paste(object$task$hps, collapse = ", "),
      ifelse(
        length(object$task$cat_hps) > 0,
        paste(object$task$cat_hps, collapse = ", "),
        "None"
      )
    )
  )

  cat("SUMMARY\n--------------------------\n")
  cat(knitr::kable(info_dt, format = "simple", align = c("l", "l")), sep = "\n")

  cat("\n\nCoefficients:\n--------------------------\n")
  cat(knitr::kable(coefs, format = "simple"), sep = "\n")

  cat("\n\nStatus:\n--------------------------\n")
  cat(
    knitr::kable(dt, format = "simple", align = c("l", "l", "r", "r", "r")),
    sep = "\n"
  )

  return(invisible(list(status = dt, coefficients = coefs, outliers = outlier)))
}
