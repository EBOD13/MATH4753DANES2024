#' Calculate a Confidence Interval for the Mean
#'
#' Computes a 95% confidence interval for the mean of a numeric vector using the Student's t-distribution.
#'
#' @importFrom stats qt sd
#' @param x A numeric vector of data values.
#'
#' @return A numeric vector containing the lower and upper bounds of the confidence interval.
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(30, mean = 5, sd = 2)
#' myci(data)
myci <- function(x) {
  if (!is.numeric(x) || length(x) < 2) {
    stop("Input 'x' must be a numeric vector with at least 2 values.")
  }
  n <- length(x)                # Sample size
  x_bar <- mean(x)              # Sample mean
  s <- sd(x)                    # Sample standard deviation
  alpha <- 0.05                 # Significance level for 95% CI
  t_critical <- qt(1 - alpha / 2, df = n - 1)  # Critical t-value

  # Calculate the margin of error
  margin_of_error <- t_critical * (s / sqrt(n))

  # Calculate the confidence interval
  lower_bound <- x_bar - margin_of_error
  upper_bound <- x_bar + margin_of_error

  # Return the confidence interval
  c(lower_bound, upper_bound)
}
