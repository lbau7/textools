#' Correlation Table
#'
#' corr.table creates a lower triangle correlation table with stars that indicate whether a
#' correlation coefficient is significantly different from zero.
#'
#' @param x a matrix or a data.frame with the variables for which the pairwise correlations should
#' be calculated.
#' @param type type of correlation coefficient that will be calculated.
#' One of "pearson" (default) and "spearman".
#' @param tex a logical value that indicates whether the results should be displayed as a LaTeX
#' table or not. Default is TRUE.
#' @param title a character variable that is used as the caption of the LaTeX table if tex is TRUE.
#' If title is NULL (default) then the title is "Correlation Table".
#' @param labs a character vector that is used for the rownames and the colnames of the table.
#' If labs is NULL (default) then the colnames of x are used for the rownames and colnames.
#' @param digits number of digits that are displayed for relative frequencies. Default is 3.
#' @param ... further arguments to be passed to other functions.
#'
#' @return \code{corr.table} uses \code{stargazer} to return LaTeX code for a table.
#' @export
corr.table <- function(x, type = c("pearson", "spearman"), tex = TRUE, title = NULL,
                       labs = NULL, digits = 3, ...) {
  type <- match.arg(type)
  x <- as.matrix(x)
  corrtab <- Hmisc::rcorr(x, type = type)
  diag(corrtab$P) <- 1
  diag(corrtab$r) <- 1
  corrstars <- with(corrtab, ifelse(P < .01, "**", ifelse(P < .05, "*", "")))
  corrtab <- round(corrtab$r, digits = digits)
  corrtab <- matrix(paste0(corrtab, corrstars), ncol=ncol(x))

  if (is.null(labs)) labs <- colnames(x)
  rownames(corrtab) <- labs
  colnames(corrtab) <- labs
  corrtab[upper.tri(corrtab)] <- ""

  if (tex == TRUE) {
    if (is.null(title)) title <- "Correlation Table"
    stargazer::stargazer(corrtab, title = title, ...)
  } else {
    corrtab
  }
}
