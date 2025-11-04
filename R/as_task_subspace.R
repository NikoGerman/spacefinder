#' @include TaskSubspace.R
#' @title sugar function to create new TaskSubspace
#' @param ... arguments passed to TaskSubsapce$new()
#' @export

as_task_subspace <- function(...) {
  TaskSubspace$new(...)
}
