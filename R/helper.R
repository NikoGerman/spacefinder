#' @keywords internal
check_packages <- function(pkgs) {
  missing <- vapply(
    pkgs,
    \(pkg) !requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )
  if (any(missing)) {
    stop(
      sprintf(
        "Package(s) %s needed for this function. Please install it/them.",
        paste0(sprintf("'%s'", pkgs[missing]), collapse = " ,")
      ),
      call. = FALSE
    )
  }
}

#' @keywords internal
resolve_selected <- function(object, select) {
  present_hps <- object$task$hps
  if (select == "all") {
    return(present_hps)
  }
  selected <- base::intersect(present_hps, select)
  if (length(selected) == 0) {
    stop("selected cols not present in task")
  }
  return(selected)
}
