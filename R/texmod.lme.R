#' LaTeX tables for lme models
#' 
#' texmod method for models of class \code{lme}.
#'
#' @param mod A model of class \code{lme}.
#' @template results
#' @template estimate_mixed
#' @template ci_linear
#' @template ci_level
#' @template se_linear
#' @template teststatistic
#' @template df
#' @template pval
#' @template intercept
#' @template title_mixed
#' @template n_title
#' @template rowlabs 
#' @template addref 
#' @template digits 
#' @template dotdotdot 
#'
#' @export
#'
#' @examples
#' library(nlme)
#' bdf.lme <- nlme::lme(IQ.verb ~ sex + aritPOST + denomina + Minority,
#'   random = ~1|schoolNR,
#'   data = nlme::bdf)
#' texmod(bdf.lme,
#'   title = "Mixed Model Regression for Verbal IQ",
#'   rowlabs = c("Sex (female)", "Sex (male)", "Arit (post)", 
#'     "School (public)", "School (protestant)", "School (catholic)",
#'     "School (private)", "Minority (no)", "Minority (yes)")
#' )
texmod.lme <- function(mod, results = c("summary", "Anova"), estimate = TRUE, 
                       ci = TRUE, ci_level = 0.95, se = FALSE, 
                       teststatistic = FALSE, df = TRUE, pval = TRUE, 
                       intercept = FALSE, title = NULL, n_title = TRUE, 
                       rowlabs = NULL, addref = TRUE, digits = 3, ...) {
  results <- match.arg(results)
  dotlist <- list(...)
  
  if (results == "summary") {
    coefsm <- coef(summary(mod))
    colnames(coefsm) <- c("Estimate", "Std.Error", "df", "t-Value", "p-Value")
    inc.col <- which(c(estimate, se, df, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
    
    if (ci == TRUE & estimate == TRUE) {
      estci <- intervals(mod, level = ci_level)$fixed
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1],
        "Upper CL" = estci[, 3], coefsm[, -1, drop = FALSE])
    }
    
    if (intercept == FALSE) coefsm <- coefsm[-1, , drop = FALSE]
    if (is.null(title)) title <- "Mixed Model Regression"
  }
  
  if (results == "Anova") {
    coefsm <- as.matrix(car::Anova(mod, ...))
    colnames(coefsm) <- c("Chisq", "df", "p-Value")
    inc.col <- which(c(teststatistic, df, pval) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
    if (is.null(title)) title <- "Mixed Model Analysis of Variance"
  }
  
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (results == "summary" & addref == TRUE) {
    nameselect <- names(mod$fixDF$terms)[-1]
    modframe <- mod$data[, nameselect, drop = FALSE]
    facrows <- sapply(modframe, class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    faclevs <- sapply(modframe[,facrows], function(x) levels(x)[1])
    facrlabs <- paste0(names(faclevs), faclevs)
    
    facvec <- numeric()
    for(i in 1:length(facrows)) {
      if (facrows[i] == FALSE) {
        facvec <- c(facvec, FALSE) 
      } else {
        facvec <- c(facvec, TRUE, rep(FALSE, length(levels(modframe[, i])) - 2))
      }
    }
    
    if (intercept == TRUE) facvec <- c(0, facvec)
    
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
    title <- paste0(title, " (n = ", nrow(model.frame(mod)), ")")
  }
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(
    stargazer::stargazer, 
    c(list(coefsm, title = title, header = FALSE), arglist.sg)
  )
}