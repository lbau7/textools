#' LaTeX tables for lm models
#' 
#' texmod method for models of class \code{lm}.
#'
#' @param mod A model of class \code{lm}.
#' @template results 
#' @template estimate
#' @template ci_linear
#' @template ci_level 
#' @template se_linear
#' @template vcov 
#' @template teststatistic
#' @template ssq
#' @template df
#' @template pval
#' @template intercept
#' @template title
#' @template n_title
#' @template rowlabs
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @return \code{texph} uses \code{stargazer} to return LaTeX code for a table.
#' @export
#' 
#' @examples
#' iris.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Species, data = iris)
#' texmod(iris.lm,
#'   title = "Linear Model for Sepal Length",
#'   rowlabs = c("Sepal Width", "Petal Width", "Species (versicolor)",
#'     "Species (setosa)", "Species (virginica)")
#' )
texmod.lm <- function(mod, results = c("summary", "Anova"), estimate = TRUE, 
                      ci = TRUE, ci_level = 0.95, se = FALSE, vcov = NULL, 
                      teststatistic = FALSE, ssq = TRUE, df = TRUE, pval = TRUE, 
                      intercept = FALSE, title = NULL, n_title = TRUE, 
                      rowlabs = NULL, addref = TRUE, digits = 3, ...) {
  results <- match.arg(results)
  dotlist <- list(...)
  
  if (results == "summary") {
    if (is.null(vcov)) {
      coefsm <- stats::coef(summary(mod))
      colnames(coefsm) <- c("Estimate", "Std.Error", "t-Value", "p-Value")
      inc.col <- which(c(estimate, se, teststatistic, pval) != 0)
      coefsm <- coefsm[, inc.col, drop = FALSE]
      
      if (ci == TRUE & estimate == TRUE) {
        estci <- stats::confint(mod, level = ci_level)
        coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1], 
          "Upper CL" = estci[, 2], coefsm[, -1, drop = FALSE])
      }
    } else {
      coefsm <- lmtest::coeftest(mod, vcov = vcov)
      colnames(coefsm) <- c("Estimate", "Std.Error", "t-Value", "p-Value")
      inc.col <- which(c(estimate, se, teststatistic, pval) != 0)
      coefsm <- coefsm[, inc.col, drop = FALSE]
      
      if (ci == TRUE & estimate == TRUE) {
        estci <- lmtest::coefci(mod, level = ci_level, vcov = vcov)
        coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1], 
          "Upper CL" = estci[, 2], coefsm[, -1, drop = FALSE])
      }
    }
    
    if (intercept == FALSE) coefsm <- coefsm[-1, , drop = FALSE]
    if (is.null(title)) title <- "Linear Model"
  }
  
  if (results == "Anova") {
    coefsm <- as.matrix(car::Anova(mod, ...))
    coefsm <- coefsm[-nrow(coefsm), , drop = FALSE]
    colnames(coefsm) <- c("Sum Sq", "df", "F-Value", "p-Value")
    inc.col <- which(c(ssq, df, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
    if (is.null(title)) title <- "Analysis of Variance"
  }
  
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (results == "summary" & addref == TRUE) {
    facrows <- sapply(stats::model.frame(mod), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    faclevs <- sapply(stats::model.frame(mod)[,facrows], 
      function(x) levels(x)[1])
    facrlabs <- paste0(names(faclevs), faclevs)
    
    facvec <- numeric()
    for(i in 1:length(facrows)) {
      if (facrows[i] == FALSE) {
        facvec <- c(facvec, FALSE) 
      } else {
        facvec <- c(facvec, TRUE, rep(FALSE, 
          length(levels(stats::model.frame(mod)[, i])) - 2))
      }
    }
    
    if (intercept == FALSE) facvec <- facvec[-1]
    
    emptyrow <- c(0, rep(".", (ncol(coefsm) - 1)))
    newrowpos <- grep(1, facvec)
    j <- 0
    for(i in 1:sum(facrows)) {
      if (newrowpos[i] == 1) {
        coefsm <- rbind(emptyrow, coefsm)
        rownames(coefsm)[1] <- facrlabs[i]
      } else {
        coefsm <- rbind(coefsm[1:(newrowpos[i] + j - 1), , drop = FALSE], 
          emptyrow,
          coefsm[(newrowpos[i] + j):nrow(coefsm), , drop = FALSE])
        rownames(coefsm)[newrowpos[i] + j] <- facrlabs[i]
      }
      j <- j + 1
    }
  }
  
  if (!is.null(rowlabs)) rownames(coefsm) <- rowlabs
  if (n_title == TRUE) {
    title <- paste0(title, " (n = ", nrow(stats::model.frame(mod)), ")")
  }
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(
    stargazer::stargazer, 
    c(list(coefsm, title = title, header = FALSE), arglist.sg)
  )
}
