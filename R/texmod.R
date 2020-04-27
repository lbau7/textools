#' LaTeX tables for regression models
#' 
#' texmod is a generic function that is used to create LaTeX tables
#' for various regression models.
#'
#' @param mod A model object for which a LaTeX table should created.
#' @template dotdotdot
#'
#' @export
texmod <- function(mod, ...) {
  UseMethod("texmod", mod)
}
