#' Compute and Visualize P-value for a t-test
#'
#' Calculates the p-value for a given test statistic in a t-distribution
#' with `n` degrees of freedom. Visualizes the t-distribution, highlighting
#' the critical regions and the observed test statistic.
#'
#' importFrom("stats", "dt", "pt")
#' @param t0 Numeric. The observed test statistic for which the p-value is calculated.
#' @param xmax Numeric. The maximum x-axis limit for plotting the t-distribution curve. Default is 4.
#' @param n Integer. The sample size, used to determine the degrees of freedom (df = n - 1). Default is 20.
#' @param alpha Numeric. The significance level for the hypothesis test. Default is 0.05.
#'
#' @return A list containing:
#' \item{q}{The critical t-value cutoff for the two-tailed test at the specified alpha level.}
#' \item{pvalue}{The two-tailed p-value for the observed test statistic `t0`.}
#'
#' @export
#'
#' @examples
#' # Example usage of mypvalue function
#' set.seed(55)
#' x1 <- rnorm(30, mean = 25, sd = 5)
#' tcalc <- (mean(x1) - 23) / (sd(x1) / sqrt(30))
#' tcalc
#' mypvalue(t0 = tcalc, n = 30)
mypvalue <- function(t0, xmax = 4, n = 20, alpha = 0.05) {
  # Input validation
  if (!is.numeric(t0) || length(t0) != 1) {
    stop("'t0' must be a single numeric value.")
  }
  if (!is.numeric(n) || n <= 1 || n != round(n)) {
    stop("'n' must be an integer greater than 1.")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a numeric value between 0 and 1.")
  }

  # Calculate p-value
  left_tail <- pt(-abs(t0), df = n - 1)
  pvalue <- 2 * left_tail

  # Plot the t-distribution curve
  curve(
    dt(x, df = n - 1), from = -xmax, to = xmax,
    ylab = "Density", xlab = expression(t),
    main = bquote("P-value = " ~ .(round(pvalue, 4)) ~ ", " ~ alpha ~ "=" ~ .(alpha)),
    col = "black", lwd = 2
  )

  # Highlight critical regions (left and right tails)
  x_right <- seq(t0, xmax, length.out = 500)
  y_right <- dt(x_right, df = n - 1)
  polygon(c(t0, x_right, xmax), c(0, y_right, 0), col = "green", border = NA)

  x_left <- seq(-xmax, -t0, length.out = 500)
  y_left <- dt(x_left, df = n - 1)
  polygon(c(-xmax, x_left, -t0), c(0, y_left, 0), col = "green", border = NA)

  # Plot critical t-values
  critical_value <- qt(1 - alpha / 2, df = n - 1)
  abline(v = c(-critical_value, critical_value), col = "blue", lty = 2, lwd = 2)
  axis(3, at = c(-critical_value, critical_value), labels = round(c(-critical_value, critical_value), 3))

  # Add text annotations for critical values and p-value
  text(critical_value, 0.02, labels = bquote(t[alpha/2] == .(round(critical_value, 3))), pos = 4, col = "blue")
  text(-critical_value, 0.02, labels = bquote(-t[alpha/2] == .(round(-critical_value, 3))), pos = 2, col = "blue")
  text(0, max(dt(seq(-xmax, xmax, length.out = 500), df = n - 1)) * 0.8,
       labels = bquote("P-value =" ~ .(round(pvalue, 4))), col = "darkgreen")

  # Return critical t-value and p-value
  return(list(q = critical_value, pvalue = pvalue))
}
