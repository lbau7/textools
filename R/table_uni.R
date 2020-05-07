#' LaTeX table for univariate frequency tables
#' 
#' \code{table_uni} creates LaTeX tables for univariate frequency
#' tables.
#'
#' @param x A categorial vector.
#' @param tex Whether the results should be displayed as a LaTeX table.
#' @param title Title for the LaTeX table.
#' @template rowlabs
#' @template digits
#' @template dotdotdot
#'
#' @return \code{table_uni} uses \code{stargazer} to return LaTeX code for a table.
#' @export
table_uni <- function(x, tex = FALSE, title = NULL, rowlabs = NULL, digits = 1, ...) {
  ntable <- table(x)
  ptable <- round(prop.table(ntable) * 100, digits = digits)
  c.table <- data.frame("n" = as.numeric(ntable), "Percent" = paste0(ptable, "%"))
  
  if(is.null(rowlabs)) rowlabs <- levels(x)
  rownames(c.table) <- rowlabs
  
  if (tex == TRUE) {
    if (is.null(title)) title <- "Frequency Table"
    stargazer::stargazer(as.matrix(c.table), title = title, ...)
  } else {
    c.table
  }
}