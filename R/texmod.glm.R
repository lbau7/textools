#' LaTeX tables for glm models
#' 
#' texmod method for models of class \code{glm}.
#'
#' @param mod A model of class \code{glm}.
#' @template results
#' @template or 
#' @template logor
#' @template ci_or 
#' @template ci_level 
#' @template se_logor
#' @template vcov
#' @template teststatistic
#' @template df
#' @template test_glm
#' @template ssq_glm
#' @template pval
#' @template intercept
#' @template title_glm
#' @template n_title
#' @template rowlabs
#' @template addref
#' @template digits
#' @template dotdotdot
#'
#' @details Models of class \code{glm} are currently only supported for 
#'   logistic regression model.
#' @export
#' 
#' @examples
#' iris$Sepal.Length <- ifelse(iris$Sepal.Length < median(iris$Sepal.Length), 0, 1)
#' iris.glm <- glm(Sepal.Length ~ Sepal.Width + Petal.Width + Species, 
#'   data = iris,
#'   family = binomial
#'   )
#' texmod(iris.glm,
#'   title = "Logistic Regression for median(Sepal Length)",
#'   rowlabs = c("Sepal Width", "Petal Width", "Species (versicolor)",
#'   "Species (setosa)", "Species (virginica)")
#' )
texmod.glm <- function(mod, results = c("summary", "Anova"), or = TRUE, 
                         logor = FALSE, ci = TRUE, ci_level = 0.95, 
                         se.logor = FALSE, vcov = NULL, teststatistic = FALSE,
                         df = TRUE, test = c("LR", "Wald", "F"), sq = TRUE, 
                         pval = TRUE, intercept = FALSE, title = NULL, 
                         n_title = TRUE, rowlabs = NULL, addref = TRUE, 
                         digits = 3, ...) {
  results <- match.arg(results)
  dotlist <- list(...)
  
  if (mod$family$family != "binomial") {
    stop("Only GLMs of family binomial are currently supported")
  }
  
  if (mod$family[2] != "logit") {
    stop("Only binomial regressions with logit-link are currently supported")
  }
  
  if (results == "summary") {
    if (is.null(vcov)) {
      coefsm <- coef(summary(mod))
      coefsm <- cbind(exp(coefsm[, 1]), coefsm)
      colnames(coefsm) <- c("Odds Ratio", "log OR", "SE (log OR)", "z-Value", "p-Value")
      inc.col <- which(c(or, logor, se.logor, teststatistic, pval) != 0)
      coefsm <- coefsm[, inc.col, drop = FALSE]
      
      if (ci == TRUE & or == TRUE) {
        estci <- exp(confint(mod, level = ci_level))
        coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1], 
          "Upper CL" = estci[,2 ], coefsm[, -1, drop = FALSE])
      }
    } else {
      coefsm <- lmtest::coeftest(mod, vcov = vcov)
      coefsm <- cbind(exp(coefsm[, 1]), coefsm)
      colnames(coefsm) <- c("Odds Ratio", "log OR", "SE (log OR)", "z-Value", "p-Value")
      inc.col <- which(c(or, logor, se.logor, teststatistic, pval) != 0)
      coefsm <- coefsm[, inc.col, drop = FALSE]
      
      if (ci == TRUE & or == TRUE) {
        estci <- exp(coefci(mod, level = ci_level, vcov = vcov))
        coefsm <- cbind(coefsm[, 1, drop = FALSE], "Lower CL" = estci[, 1], 
          "Upper CL" = estci[,2 ], coefsm[, -1, drop = FALSE])
      }
    }
    
    if (intercept == FALSE) coefsm <- coefsm[-1, , drop = FALSE]
    if (is.null(title)) title <- "Logistic Regression"
  }
  
  
  if (results == "Anova") {
    test <- match.arg(test)
    coefsm <- as.matrix(car::Anova(mod, test.statistic = test, ...))
    
    if (test == "LR") {
      colnames(coefsm) <- c("LR Chisq", "df", "p-Value")
      inc.col <- which(c(teststatistic, df, pval) != 0)
      coefsm <- coefsm[, inc.col, drop = FALSE]
    } else if (test == "F") {
      colnames(coefsm) <- c("Sum Sq", "df", "F-Value", "p-Value")
      inc.col <- which(c(ssq, df, teststatistic, pval) != 0)
      coefsm <- coefsm[-nrow(coefsm), inc.col, drop = FALSE]
    } else if (test == "Wald") {
      colnames(coefsm) <- c("df", "Chisq", "p-Value")
      inc.col <- which(c(df, teststatistic, pval) != 0)
    }
    if (is.null(title)) title <- "Binomial Analysis of Variance"
  }
  
  if (pval == TRUE) highsig <- which(coefsm[, ncol(coefsm)] < 0.001)
  coefsm <- round(coefsm, digits = digits)
  if (pval == TRUE) coefsm[highsig, ncol(coefsm)] <- "<0.001"
  
  if (results == "summary" & addref == TRUE) {
    facrows <- sapply(model.frame(mod), class)
    facrows <- sapply(facrows, function(x) x[[1]])
    facrows <- facrows %in% c("factor", "ordered")
    facrows[1] <- FALSE
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
    title <- paste0(title, " (n = ", nrow(model.frame(mod)), ")")
  }
  arglist.sg <- dotlist[names(dotlist) == "table.placement"]
  do.call(
    stargazer::stargazer, 
    c(list(coefsm, title = title, header = FALSE), arglist.sg)
  )
}