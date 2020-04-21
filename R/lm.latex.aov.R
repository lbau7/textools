lm.latex.aov <- function(mod, results = c("summary", "Anova"), ssq = TRUE, meansq = TRUE, 
  df = TRUE, teststatistic = FALSE, pval = TRUE, title = NULL, 
  rowlabs = NULL, digits = 3, ...) {
  results <- match.arg(results)
  dotlist <- list(...)
  
  if (results == "summary") {
    coefsm <- as.matrix(summary(mod)[[1]])
    coefsm <- coefsm[-nrow(coefsm), , drop = FALSE]
    colnames(coefsm) <- c("df", "Sum Sq", "Mean Sq", "F-Value", "p-Value")
    inc.col <- which(c(df, ssq, meansq, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
  }
  
  if (results == "Anova") {
    coefsm <- as.matrix(car::Anova(mod, ...))
    coefsm <- coefsm[-nrow(coefsm), , drop = FALSE]
    colnames(coefsm) <- c("Sum Sq", "df", "F-Value", "p-Value")
    inc.col <- which(c(ssq, df, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc.col, drop=F]
  }
  
  if (is.null(title)) title <- "Analysis of Variance"
  
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs
  tabcaption <- paste0(title, " (n = ", nrow(model.frame(mod)), ")")
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(stargazer::stargazer, c(list(coefsm, title = tabcaption), arglist.sg))
}