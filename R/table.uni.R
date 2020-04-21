table.uni <- function(x, tex = FALSE, title = NULL, rowlabs = NULL, digits = 1, ...) {
  ntable <- table(x)
  ptable <- round(prop.table(ntable) * 100, digits = digits)
  c.table <- data.frame("n" = as.numeric(ntable), "Percent" = paste0(ptable, "%"))
  
  if(is.null(rowlabs)) rowlabs <- levels(x)
  rownames(c.table) <- rowlabs
  
  if (tex == TRUE) {
    if (is.null(title)) title <- "Frequency Table"
    stargazer(as.matrix(c.table), title = title, ...)
  } else {
    c.table
  }
}