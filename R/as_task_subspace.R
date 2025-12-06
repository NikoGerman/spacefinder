#' @include TaskSubspace.R
#' @title Create a subspace task
#' @description
#' Convenience function to create a \code{TaskSubspace} object. This is a
#' wrapper around \code{TaskSubspace$new()}.
#'
#' @param ... Arguments passed to \code{\link{TaskSubspace}$new()}. See
#'   \code{\link{TaskSubspace}} for details on available parameters.
#'
#' @return A \code{TaskSubspace} object
#'
#' @seealso \code{\link{TaskSubspace}} for full documentation and examples.
#'
#' @examples
#' \dontrun{
#' # Explicit specification
#' task <- as_task_subspace(
#'   data = benchmark_data,
#'   target_measure = "auc",
#'   hps = c("learning_rate", "max_depth")
#' )
#'
#' # Formula interface
#' task <- as_task_subspace(
#'   data = benchmark_data,
#'   formula = auc ~ (learning_rate + max_depth) * optimizer
#' )
#' }
#'
#' @export
as_task_subspace <- function(...) {
  TaskSubspace$new(...)
}
