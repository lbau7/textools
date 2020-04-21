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
    stargazer(c.table, title = title, ...)
  } else {
    c.table
  }
}