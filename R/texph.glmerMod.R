#' Post-hoc LaTeX tables for glmerMod models
#'
#' texph method for models of class \code{glmerMod}.
#'
#' @param mod A model of class \code{glmerMod}.
#' @template variable_or 
#' @template pairwise_or 
#' @template estimate_ph_or 
#' @template se_ph
#' @template teststatistic_ph 
#' @template pval_ph 
#' @template ci_ph 
#' @template ci_level 
#' @template title_ph_or 
#' @template varlab 
#' @template rowlabs 
#' @template digits 
#' @template dotdotdot 
#'
#' @return \code{texph} uses \code{stargazer} to return LaTeX code for a table.
#' @export
#'
#' @examples
#' bdf <- nlme::bdf
#' bdf$IQ.verb <- ifelse(bdf$IQ.verb < median(bdf$IQ.verb), 0, 1)
#' bdf.glmerMod <- lme4::glmer(IQ.verb ~ sex + aritPOST + denomina +
#'   (1|schoolNR), 
#'   data = bdf, 
#'   family = "binomial"
#' )
#' texph(bdf.glmerMod,
#'   variable = "denomina",
#'   rowlabs = c("Public / Protestant", "Public / Catholic", "Public / Private",
#'     "Protestant / Catholic", "Protestant / Private", "Catholic / Private"),
#'   title = "Pairwise Odds Ratios for median(Verbal IQ)"
#' )
#' 
#' texph(bdf.glmerMod,
#'   variable = "denomina",
#'   pairwise = FALSE,
#'   varlab = "Denomina",
#'   rowlabs = c("Public", "Protestant", "Catholic", "Private"),
#'  title = "EM Probabilities for median(Verbal IQ)"
#' )
texph.glmerMod <- function(mod, variable, pairwise = TRUE, estimate = TRUE,
                           se = FALSE, teststatistic = FALSE, pval = TRUE,
                           ci = TRUE, ci_level = 0.95, title = NULL, 
                           varlab = NULL, rowlabs = NULL, digits = 3, ...) {
  dotlist <- list(...)
  emmod <- emmeans::emmeans(mod, variable, type = "response")
  
  if (pairwise == TRUE) {
    emmod <- graphics::pairs(emmod, ...)
    coefem <- data.frame(emmod)[, -4]
    colnames(coefem) <- c("Contrast", "Odds Ratio", "Std. Error", "z-Ratio", "p-Value")
    inc.col <- which(c(estimate, se, teststatistic, pval) != 0) + 1
    coefem <- coefem[, c(1, inc.col), drop = FALSE]
    
    if(ci == TRUE & estimate == TRUE) {
      emci <- stats::confint(emmod, level = ci_level)[, 5:6]
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
  do.call(
    stargazer::stargazer,
    c(list(as.matrix(coefem), title = title, header = FALSE), arglist.sg)
  )
}
