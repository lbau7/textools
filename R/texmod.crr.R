#' LaTeX tables for crr models
#' 
#' texmod method for models of class \code{crr}.
#'
#' @param mod A model of class \code{crr}.
#' @template hr 
#' @template loghr 
#' @template ci_hr 
#' @template ci_level 
#' @template se_loghr 
#' @template teststatistic 
#' @template pval 
#' @template title_crr
#' @template rowlabs
#' @template digits
#' @template dotdotdot
#'
#' @return \code{texph} uses \code{stargazer} to return LaTeX code for a table.
#' @export
texmod.crr <- function(mod, hr = TRUE, loghr = FALSE, ci = TRUE, 
                       ci_level = 0.95, se_loghr = FALSE, 
                       teststatistic = FALSE, pval = TRUE,
                       title = NULL, rowlabs = NULL, digits = 3,
                       ...) {
  dotlist <- list(...)
  
  if (is.null(rowlabs)) rowlabs <- rownames(summary(mod)$coef)
  coefsm <- matrix(summary(mod)$coef[, c(2, 1, 3:5)], ncol = 5)
  colnames(coefsm) <- c("Hazard Ratio", "log HR", "SE (log HR)", "z-Value", "p-Value")
  rownames(coefsm) <- rowlabs
  
  inc.col <- which(c(hr, loghr, se_loghr, teststatistic, pval) != 0)
  coefsm <- coefsm[, inc.col, drop = FALSE]
  
  if (ci == TRUE & hr == TRUE) {
    ci.low <- summary(mod)$conf.int[, 3]
    ci.up <- summary(mod)$conf.int[, 4]
    coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = ci.low, "Upper CL" = ci.up,
      coefsm[, -1, drop = FALSE])
  }
  
  if (is.null(title)) title <- "Competing Risks Regression"
  
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(stargazer::stargazer, c(list(coefsm, title = title), arglist.sg))
}
