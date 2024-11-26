#' Bootstrap Confidence Interval
#'
#' @param iter Number of bootstrap iterations
#' @param x Input data vector
#' @param fun Function to apply (e.g., "mean", "median")
#' @param alpha Significance level for the confidence interval
#' @param cx Text size for annotations
#' @param ... Additional arguments passed to the histogram function
#'
#' @importFrom stats quantile sd
#' @importFrom graphics hist segments text
#'
#' @return A list containing the confidence interval (`ci`), the function applied (`fun`), and the input data (`x`)
#' @export
myboot2 <- function(iter = 10000, x, fun = mean, alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))

  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap Sample Statistics", "\n",
                            "alpha=", alpha, " iter=", iter, sep = ""),
               ...)
  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)

  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x))
}
