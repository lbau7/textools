#' LaTeX tables for aov models
#' 
#' texmod method for models of class \code{aov}.
#'
#' @param mod A model of class \code{aov}. 
#' @template results
#' @template ssq
#' @template meansq
#' @template df
#' @template teststatistic
#' @template pval
#' @template title_aov
#' @template n_title
#' @template rowlabs 
#' @template digits 
#' @template dotdotdot 
#'
#' @export
#' 
#' @examples
#' iris.aov <- aov(Sepal.Length ~ Sepal.Width + Petal.Width + Species, data = iris)
#' texmod(iris.aov,
#'  title = "Analysis of Variance for Sepal Length",
#'  rowlabs = c("Sepal Width", "Petal Width", "Species")
#' )
texmod.aov <- function(mod, results = c("summary", "Anova"), ssq = TRUE, 
                         meansq = TRUE, df = TRUE, teststatistic = FALSE, 
                         pval = TRUE, title = NULL, n_title = TRUE,
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
  if (n_title == TRUE) {
    title <- paste0(title, " (n = ", nrow(model.frame(mod)), ")")
  }
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(
    stargazer::stargazer, 
    c(list(coefsm, title = title, header = FALSE), arglist.sg)
  )
}