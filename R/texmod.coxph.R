#' LaTeX tables for coxph models
#' 
#' texmod method for models of class \code{coxph}.
#'
#' @param mod A model of class \code{coxph}.
#' @template results
#' @template hr
#' @template loghr
#' @template ci_hr
#' @template ci_level
#' @template se_loghr 
#' @template teststatistic
#' @template df
#' @template test_coxph
#' @template pval
#' @template title_cox
#' @template n_title
#' @template rowlabs
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @export
#' 
#' @examples
#' library(survival)
#' lung.coxph <- coxph(Surv(time, status) ~ age + factor(sex) + meal.cal + ph.karno,
#'   data = lung)
#' texmod(lung.coxph,
#'   title = "Cox Regression for Overall Survival",
#'   rowlabs = c("Age", "Sex (male)", "Sex (female)", "Meal calories",
#'     "Karnofsky score (physician)")
#'  )
texmod.coxph <- function(mod, results = c("summary", "Anova"), hr = TRUE, 
                         loghr = FALSE, ci = TRUE, ci_level = 0.95, 
                         se_loghr = FALSE, teststatistic = FALSE, df = TRUE,
                         test = c("LR", "Wald"), pval = TRUE, title = NULL, 
                         n_title = TRUE, rowlabs = NULL, addref = TRUE, 
                         digits = 3, ...) {
  results <- match.arg(results)
  dotlist <- list(...)
  
  if (results == "summary") {
    sm <- summary(mod, conf.int = ci_level)
    coefsm <- coef(sm)[, c(2, 1, 3:5), drop = FALSE]
    colnames(coefsm) <- c("Hazard Ratio", "log HR", "SE (log HR)", "z-Value", "p-Value")
    inc.col <- which(c(hr, loghr, se_loghr, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
    
    if (ci == TRUE & hr == TRUE) {
      ci.low <- sm$conf.int[,3]
      ci.up <- sm$conf.int[,4]
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = ci.low, 
        "Upper CL" = ci.up, coefsm[, -1, drop = FALSE])
    }
    
    if (is.null(title)) title <- "Cox Regression"
  }
  
  if (results == "Anova") {
    test <- match.arg(test)
    coefsm <- as.matrix(car::Anova(mod, test.statistic = test, ...))
    
    if (test == "LR") {
      colnames(coefsm) <- c("LR Chisq", "df", "p-Value")
      inc.col <- which(c(teststatistic, df, pval) != 0)
      coefsm <- coefsm[, inc.col, drop = FALSE]
    } else if (test == "Wald") {
      colnames(coefsm) <- c("df", "Chisq", "p-Value")
      inc.col <- which(c(df, teststatistic, pval) != 0)
      coefsm <- coefsm[, inc.col, drop = FALSE]
    }
    
    if (is.null(title)) title <- "Cox Analysis of Variance"
  }
  
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (results == "summary" & addref == TRUE) {
    facrows <- sapply(model.frame(mod), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    faclevs <- sapply(model.frame(mod)[,facrows], function(x) levels(x)[1])
    facrlabs <- paste0(names(faclevs), faclevs)
    
    facvec <- numeric()
    for(i in 1:length(facrows)) {
      if (facrows[i] == FALSE) {
        facvec <- c(facvec, FALSE) 
      } else {
        facvec <- c(facvec, TRUE, rep(FALSE, length(levels(model.frame(mod)[, i])) - 2))
      }
    }
    
    facvec <- facvec[-1]
    if (sum(facvec != 0)) { 
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