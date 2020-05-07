#' LaTeX tables for glmerMod models
#' 
#' texmod method for models of class \code{glmerMod}.
#'
#' @param mod A model of class \code{glmerMod}.
#' @template results 
#' @template or 
#' @template logor 
#' @template ci_or 
#' @template ci_level 
#' @template se_logor 
#' @template teststatistic 
#' @template df 
#' @template pval 
#' @template intercept 
#' @template title_glmmixed 
#' @template n_title
#' @template rowlabs 
#' @template addref 
#' @template digits 
#' @template dotdotdot 
#'
#' @return \code{texph} uses \code{stargazer} to return LaTeX code for a table.
#' @details Models of class \code{glmerMod} are currently only supported for 
#'   logistic mixed model regressions.
#' @export
#'
#' @examples
#' bdf <- nlme::bdf
#' bdf$IQ.verb <- ifelse(bdf$IQ.verb < median(bdf$IQ.verb), 0, 1)
#' bdf.glmerMod <- lme4::glmer(IQ.verb ~ sex + aritPOST + Minority +
#'    (1|schoolNR), data = bdf, family = "binomial")
#' texmod(bdf.glmerMod,
#'   method = "Wald",
#'   title = "Logistic Mixed Model Regression for Verbal IQ",
#'   rowlabs = c("Sex (female)", "Sex (male)", "Arit (post)",
#'     "Minority (no)", "Minority (yes)")
#' )
texmod.glmerMod <- function(mod, results = c("summary", "Anova"), or = TRUE, 
                            logor = FALSE, ci = TRUE, ci_level = 0.95, 
                            se_logor = FALSE, teststatistic = FALSE, 
                            df = TRUE, pval = TRUE, intercept = FALSE, 
                            title = NULL, n_title = TRUE, rowlabs = NULL, 
                            addref = TRUE, digits = 3, ...) {
  results <- match.arg(results)
  dotlist <- list(...)
  
  if (mod@resp$family$family != "binomial") {
    stop("Only GLMs of family binomial are currently supported")
  }
  
  if (mod@resp$family$link != "logit") {
    stop("Only binomial regressions with logit-link are currently supported")
  }
  
  if (results == "summary") {
    coefsm <- stats::coef(summary(mod))
    coefsm <- cbind(exp(coefsm[, 1]), coefsm)
    colnames(coefsm) <- c("Odds Ratios", "log OR", "SE (log OR)", "z-Value", "p-Value")
    inc.col <- which(c(or, logor, se_logor, teststatistic, pval) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
    
    if (ci == TRUE & or == TRUE) {
      arglist.ci <- dotlist[names(dotlist) %in% c("method", "boot.type")]
      estci <- do.call(stats::confint, c(list(mod, level = ci_level), arglist.ci))
      estci <- exp(estci)
      estci <- estci[(nrow(estci) - nrow(coefsm) + 1):nrow(estci), , drop = FALSE]
      coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1], 
        "Upper CL" = estci[, 2], coefsm[, -1, drop = FALSE])
    }
    
    if (intercept == FALSE) coefsm <- coefsm[-1, , drop = FALSE]
    
    if (is.null(title)) title <- "Logistic Mixed Model Regression"
  }
  
  if (results == "Anova") {
    coefsm <- as.matrix(car::Anova(mod, ...))
    colnames(coefsm) <- c("Chisq", "df", "p-Value")
    inc.col <- which(c(teststatistic, df, pval) != 0)
    coefsm <- coefsm[, inc.col, drop = FALSE]
    
    if (is.null(title)) title <- "Binomial Mixed Model ANOVA"
  }
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (results == "summary" & addref == TRUE) {
    modframe <- mod@frame[, -c(1, ncol(mod@frame)), drop = FALSE]
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
    title <- paste0(title, " (n = ", nrow(stats::model.frame(mod)), ")")
  }
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(
    stargazer::stargazer,
    c(list(coefsm, title = title, header = FALSE), arglist.sg)
  )
}