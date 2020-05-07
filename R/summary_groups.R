#' LaTeX tables for summaries by groups
#' 
#' \code{summary_groups} generates a LaTeX table with the descriptive
#' summary of a continuous variable by groups.
#'
#' @param x The variable for which a summary is computed.
#' @param groups A group variable. 
#' @param sum.all Whether a summary over all groups should be added.
#' @template rowlabs
#' @param tex Whether the output should be TeX code. 
#' @template title_descr
#' @template digits
#' @template dotdotdot
#'
#' @return \code{summary_groupss} uses \code{stargazer} to return 
#'   LaTeX code for a table.
#' @export
summary_groups <- function(x, groups, sum.all = TRUE, rowlabs = NULL, 
                           tex = FALSE, title = NULL, digits = 2, ...) {
  summary.df <- stats::aggregate(x, list(groups), summary_new)[,2 ]
  if (!is.factor(groups)) groups <- factor(groups)
  if (is.null(rowlabs)) rowlabs <- levels(groups)
  if (sum.all == TRUE) {
    summary.df <- rbind(summary_new(x), summary.df)
    rownames(summary.df) <- c("All", rowlabs)
  } else {
    rownames(summary.df) <- rowlabs
  }
  
  if (tex == TRUE) {
    if (is.null(title)) title <- "Descriptive Summary"
    stargazer::stargazer(as.matrix(summary.df), 
      title = title, 
      digits = digits,
      ...)
  } else {
    summary.df
  }
}