summary.groups <- function(x, groups, sum.all = TRUE, rowlabs = NULL, 
  tex = FALSE, title = NULL, digits = 2, ...) {
  summary.df <- aggregate(x, list(groups), summary.new)[,2 ]
  if (!is.factor(groups)) groups <- factor(groups)
  if (is.null(rowlabs)) rowlabs <- levels(groups)
  if (sum.all == TRUE) {
    summary.df <- rbind(summary.new(x), summary.df)
    rownames(summary.df) <- c("All", rowlabs)
  } else {
    rownames(summary.df) <- rowlabs
  }
  
  if (tex == TRUE) {
    if (is.null(title)) title <- "Descriptive Summary"
    stargazer(as.matrix(summary.df), 
      title = title, 
      digits = digits,
      ...)
  } else {
    summary.df
  }
}