#' Contingency Table
#' 
#' \code{ctable} creates a contingency table with absolute and relative
#' frequencies.
#' 
#'
#' @param x A vector that represents the variable displayed in rows.
#' @param y A vector that represents the variable displayed in columns.
#' @param margin Whether the relative frequencies should be computed rowwise
#'   or columnwise
#' @param tex Whether the results should be displayed as a LaTeX table.
#' @param title Title for the LaTeX table.
#' @template rowlabs
#' @param collabs Names for the columns.
#' @template digits
#' @template dotdotdot
#'
#' @return \code{ctable} uses \code{stargazer} to return LaTeX code for a table.
#' @export
ctable <- function(x, y, margin = 2, tex = FALSE, title = NULL, 
  rowlabs = NULL, collabs = NULL, digits = 1, ...) {
  ntable <- table(x, y)
  ptable <- round(prop.table(ntable, margin = margin) * 100, digits = digits)
  c.table <- matrix(paste(ntable, " (", ptable, "%)", sep=""), nrow = nrow(ptable))
  
  if (is.null(rowlabs)) rowlabs <- levels(x)
  if (is.null(collabs)) collabs <- levels(y)
  rownames(c.table) <- rowlabs
  colnames(c.table) <- collabs
  
  if (tex == TRUE) {
    if (is.null(title)) title <- "Contingency Table"
    stargazer::stargazer(c.table, title = title, ...)
  } else {
    c.table
  }
}