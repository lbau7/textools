ph.latex.lm <- function(mod, variable, pairwise = TRUE, estimate = TRUE, se = FALSE, 
  df = TRUE, teststatistic = FALSE, pval = TRUE, ci = TRUE, 
  ci.level = 0.95, title = NULL, varlab = NULL, rowlabs = NULL, 
  digits = 3, ...) {
  dotlist <- list(...)
  emmod <- emmeans::emmeans(mod, variable)
  
  if (pairwise == TRUE) {
    emmod <- pairs(emmod, ...)
    coefem <- data.frame(emmod)
    colnames(coefem) <- c("Contrast", "Mean Diff", "Std. Error", "df", "t-Ratio", "p-Value")
    inc.col <- which(c(estimate, se, df, teststatistic, pval) != 0) + 1
    coefem <- coefem[, c(1, inc.col), drop = FALSE]
    
    if(ci == TRUE) {
      emci <- confint(emmod, level = ci.level)[, 5:6]
      coefem <- cbind(coefem[, 1:2, drop = FALSE], "Lower CL" = emci[, 1], 
        "Upper CL" = emci[,2 ], coefem[, -(1:2), drop = FALSE])
    }
    
    if(is.null(title)) title <- "Pairwise Comparisons"
  }
  
  if (pairwise == FALSE) {
    coefem <- data.frame(emmod)
    if (is.null(varlab)) varlab <- variable
    colnames(coefem) <- c(varlab, "EM Mean", "Std.Error", "df", "Lower CL", "Upper CL")
    inc.col <- which(c(estimate, se, df, ci, ci) != 0) + 1
    coefem <- coefem[, c(1, inc.col), drop = FALSE]
    if(is.null(title)) title <- "EM Means"
  }
  
  if(pval == TRUE) highsig <- which(coefem[, ncol(coefem)] < 0.001)
  coefem[, 2:ncol(coefem)] <- round(coefem[, 2:ncol(coefem)], digits = digits)
  if(pval == TRUE) coefem[highsig, ncol(coefem)] <- "<0.001"
  
  if (!is.null(rowlabs)) coefsm[, 1] <- rowlabs
  rownames(coefem) <- NULL
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(stargazer::stargazer, c(list(as.matrix(coefem), title = title), arglist.sg))
}