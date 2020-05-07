#' Post-hoc LaTeX tables for aov models
#' 
#' texph method for models of class \code{aov}.
#'
#' @param mod A model of class \code{aov}.
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
#' @details The method for the multiple comparison adjustments for 
#'   both the p-values and the confidence intervals (only for pairwise 
#'   comparisons) can be changed by passing the \code{adjust} argument of 
#'   \code{emmeans} function through the \code{...} argument. The same
#'   adjustment method will be used for both p-values and confidence intervals. 
#'   The default method is Tukey’s method. Other possibilities are \code{"scheffe"}, 
#'   \code{"sidak"}, \code{"bonferroni"}, \code{"dunnettx"}, \code{"mvt"}, and 
#'   \code{"none"}. Please refer to the reference manual of the library emmeans for 
#'   details. Note that some other adjustment methods are available in emmeans for 
#'   p-values but not for confidence intervals. If these methods are used in
#'   \code{texph} the p-values will be adjusted by the correct method but the 
#'   simultaneous confidence intervals will be calculated using Bonferroni’s method 
#'   without producing a warning.
#' @export
#' 
#' @examples
#' iris.aov <- aov(Sepal.Length ~ Sepal.Width + Petal.Width + Species, 
#'   data = iris)
#' texph(iris.aov,
#'   variable = "Species",
#'   title = "Pairwise Comparisons for Sepal Length"
#' )
#' 
#' texph(iris.aov,
#'   pairwise = FALSE,
#'   variable = "Species",
#'   title = "EM Means for Sepal Length"
#' )
texph.aov <- function(mod, variable, pairwise = TRUE, estimate = TRUE, 
                      se = FALSE, df = TRUE, teststatistic = FALSE, 
                      pval = TRUE, ci = TRUE, ci_level = 0.95, title = NULL, 
                      varlab = NULL, rowlabs = NULL, digits = 3, ...) {
  dotlist <- list(...)
  emmod <- emmeans::emmeans(mod, variable)
  
  if (pairwise == TRUE) {
    emmod <- graphics::pairs(emmod, ...)
    coefem <- data.frame(emmod)
    colnames(coefem) <- c("Contrast", "Mean Diff", "Std. Error", "df", "t-Ratio", "p-Value")
    inc.col <- which(c(estimate, se, df, teststatistic, pval) != 0) + 1
    coefem <- coefem[, c(1, inc.col), drop = FALSE]
    
    if(ci == TRUE) {
      emci <- stats::confint(emmod, level = ci_level)[, 5:6]
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