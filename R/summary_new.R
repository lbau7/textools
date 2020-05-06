summary_new <- function(x, tex = FALSE, title = NULL, digits = 2,  ...) {
  N.x <- sum(!is.na(x))
  nmiss.x <- sum(is.na(x))
  median.x <- stats::median(x, na.rm = TRUE)
  quartil1.x <- as.numeric(stats::quantile(x, probs = 0.25, na.rm = TRUE))
  quartil3.x <- as.numeric(stats::quantile(x, probs = 0.75, na.rm = TRUE))
  mean.x <- mean(x, na.rm = TRUE)
  sd.x <- stats::sd(x, na.rm = TRUE)
  min.x <- min(x, na.rm = TRUE)
  max.x <- max(x, na.rm = TRUE)
  
  summary.frame <- cbind("N" = N.x, "nmiss" = nmiss.x, "Mean" = mean.x, 
    "Median" = median.x, "SD" = sd.x, "min" = min.x, "Q1" = quartil1.x, 
    "Q3" = quartil3.x, "max" = max.x)
  summary.frame <- round(summary.frame, digits = digits)
  
  if (tex == TRUE) {
    if (is.null(title)) title <- "Descriptive Summary"
    stargazer::stargazer(as.matrix(summary.frame), title = title, ...)
  } else {
    summary.frame
  }
}