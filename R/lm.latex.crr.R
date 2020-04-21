lm.latex.crr <- function(mod, hr = TRUE, log.hr = FALSE, ci = TRUE, ci.level = 0.95,
  se.log.hr = FALSE, teststatistic = FALSE, pval = TRUE,
  title = NULL, rowlabs = NULL, digits = 3, ...) {
  dotlist <- list(...)
  
  if (is.null(rowlabs)) rowlabs <- rownames(summary(mod)$coef)
  coefsm <- matrix(summary(mod)$coef[, c(2, 1, 3:5)], ncol = 5)
  colnames(coefsm) <- c("Hazard Ratio", "log HR", "SE (log HR)", "z-Value", "p-Value")
  rownames(coefsm) <- rowlabs
  
  inc.col <- which(c(hr, log.hr, se.log.hr, teststatistic, pval) != 0)
  coefsm <- coefsm[, inc.col, drop = FALSE]
  
  if (ci == TRUE & hr == TRUE) {
    ci.low <- summary(mod)$conf.int[, 3]
    ci.up <- summary(mod)$conf.int[, 4]
    coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = ci.low, "Upper CL" = ci.up,
      coefsm[, -1, drop = FALSE])
  }
  
  if (is.null(title)) title <- "Competing Risk Regression"
  
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(stargazer::stargazer, c(list(coefsm, title = title), arglist.sg))
}