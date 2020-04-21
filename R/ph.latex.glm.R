ph.latex.glm <- function(mod, variable, pairwise = TRUE, estimate = TRUE, se = FALSE, 
  teststatistic = FALSE, pval = TRUE, ci = TRUE, ci.level = 0.95,
  title = NULL, varlab = NULL, rowlabs = NULL, digits = 3, ...) {
  dotlist <- list(...)
  emmod <- emmeans::emmeans(mod, variable, type = "response")
  
  if (pairwise == TRUE) {
    emmod <- pairs(emmod, ...)
    coefem <- data.frame(emmod)[, -4]
    colnames(coefem) <- c("Contrast", "Odds Ratio", "Std. Error", "z-Ratio", "p-Value")
    inc.col <- which(c(estimate, se, teststatistic, pval) != 0) + 1
    coefem <- coefem[, c(1, inc.col), drop = FALSE]
    
    if(ci == TRUE & estimate == TRUE) {
      emci <- confint(emmod, level = ci.level)[, 5:6]
      coefem <- cbind(coefem[, 1:2, drop = FALSE], "Lower CL" = emci[, 1], 
        "Upper CL" = emci[,2 ], coefem[, -(1:2), drop = FALSE])
    }
    
    if(is.null(title)) title <- "Pairwise Odds Ratios"
  }
  
  if (pairwise == FALSE) {
    coefem <- data.frame(emmod)[, -4]
    if (is.null(varlab)) varlab <- variable
    colnames(coefem) <- c(varlab, "EM Prob.", "Std.Error", "Lower CL", "Upper CL")
    inc.col <- which(c(estimate, se, ci, ci) != 0) + 1
    coefem <- coefem[, c(1, inc.col), drop = FALSE]
    if(is.null(title)) title <- "EM Probabilities"
  }
  
  if(pval == TRUE) highsig <- which(coefem[, ncol(coefem)] < 0.001)
  coefem[, 2:ncol(coefem)] <- round(coefem[, 2:ncol(coefem)], digits = digits)
  if(pval == TRUE) coefem[highsig, ncol(coefem)] <- "<0.001"
  
  if (!is.null(rowlabs)) coefem[, 1] <- rowlabs
  rownames(coefem) <- NULL
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(stargazer::stargazer, c(list(as.matrix(coefem), title = title), arglist.sg))
}