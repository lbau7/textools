#' LaTeX tables for post-hoc comparisons
#' 
#' texph is a generic functions that creates LaTeX tables for post-hoc
#' comparisons and estimated marginal means of various regression models.
#'
#' @param mod A model object for which a LaTeX table should created.
#' @template dotdotdot
#'
#' @return \code{texph} uses \code{stargazer} to return LaTeX code for a table.
#' @export
texph <- function(mod, ...) {
  UseMethod("texph", mod)
}
