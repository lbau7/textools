#' LaTeX tables for regression models
#' 
#' texmod is a generic function that creates LaTeX tables
#' for various regression models.
#'
#' @param mod A model object for which a LaTeX table should created.
#' @template dotdotdot
#' 
#' @return \code{texph} uses \code{stargazer} to return LaTeX code for a table.
#' @export
texmod <- function(mod, ...) {
  UseMethod("texmod", mod)
}
