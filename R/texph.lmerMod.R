#' Post-hoc LaTeX tables for lmerMod models
#' 
#' texph method for models of class \code{lmerMod}.
#'
#' @param mod A model of class \code{lmerMod}.
#' @template variable
#' @template pairwise
#' @template estimate_ph
#' @template se_ph 
#' @template df_ph
#' @template teststatistic_ph 
#' @template pval_ph 
#' @template ci_ph 
#' @template ci_level 
#' @template title_ph 
#' @template varlab 
#' @template rowlabs 
#' @template digits 
#' @template dotdotdot 
#'
#' @return \code{texph} uses \code{stargazer} to return LaTeX code for a table.
#' @export
#'
#' @examples
#' bdf.lmerMod <- lme4::lmer(IQ.verb ~ sex + aritPOST + denomina + 
#'   Minority + (1|schoolNR),
#'   data = nlme::bdf)
#' texph(bdf.lmerMod,
#'   variable = "denomina",
#'   rowlabs = c("Public - Protestant", "Public - Catholic", "Public - Private",
#'     "Protestant - Catholic", "Protestant - Private", "Catholic - Private"),
#'   title = "Pairwise Comparisons for Verbal IQ"
#' )
#' 
#' texph(bdf.lmerMod,
#'   variable = "denomina",
#'   pairwise = FALSE,
#'   varlab = "Denomina",
#'   title = "EM Means for Verbal IQ"
#' )
texph.lmerMod <- function(mod, variable, pairwise = TRUE, estimate = TRUE, 
                          se = FALSE, df = TRUE, teststatistic = FALSE, 
                          pval = TRUE, ci = TRUE, ci_level = 0.95, title = NULL, 
                          varlab = NULL, rowlabs = NULL, digits = 3, ...) {
  dotlist <- list(...)
  emmod <- emmeans::emmeans(mod, variable)
  
  if (pairwise == TRUE) {
    emmod <- pairs(emmod, ...)
    coefem <- data.frame(emmod)
    colnames(coefem) <- c("Contrast", "Mean Diff", "Std. Error", "df", "t-Ratio", "p-Value")
    inc.col <- which(c(estimate, se, df, teststatistic, pval) != 0) + 1
    coefem <- coefem[, c(1, inc.col), drop = FALSE]
    
    if(ci == TRUE) {
      emci <- confint(emmod, level = ci_level)[, 5:6]
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
  
  if (pval == TRUE) highsig <- which(coefem[, ncol(coefem)] < 0.001)
  coefem[, 2:ncol(coefem)] <- round(coefem[, 2:ncol(coefem)], digits = digits)
  if (df == TRUE) coefem[, "df"] <- round(coefem[, "df"], 1)
  if (pval == TRUE) coefem[highsig, ncol(coefem)] <- "<0.001"
  
  if (!is.null(rowlabs)) coefem[, 1] <- rowlabs
  rownames(coefem) <- NULL
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(stargazer::stargazer, c(list(as.matrix(coefem), title = title), arglist.sg))
}  