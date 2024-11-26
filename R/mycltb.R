#' Central Limit Theorem Simulation for Binomial Distribution
#'
#' This function simulates the Central Limit Theorem (CLT) by generating random samples
#' from a Binomial distribution, computing sample means, and comparing their distribution
#' to the theoretical normal curve.
#'
#' @param n Integer. The sample size for each iteration of random sampling (must be > 0).
#' @param iter Integer. The number of iterations for generating random samples (must be > 0).
#' @param p Numeric. The probability of success for the Binomial distribution. Must be between 0 and 1. Default is 0.5.
#' @param ... Additional graphical parameters passed to the `hist` function (e.g., `breaks`, `col`).
#'
#' @return A histogram of sample means with an overlaid normal curve for comparison.
#' @export
#'
#' @importFrom stats rbinom dnorm
#' @importFrom graphics hist curve
#'
#' @examples
#' # Simulate CLT with sample size 30, 1000 iterations, and p = 0.5
#' mycltb(30, 1000, p = 0.5)
mycltb <- function(n, iter, p = 0.5, ...) {
  # Input validation
  if (!is.numeric(n) || n <= 0 || n != round(n)) {
    stop("'n' must be a positive integer.")
  }
  if (!is.numeric(iter) || iter <= 0 || iter != round(iter)) {
    stop("'iter' must be a positive integer.")
  }
  if (!is.numeric(p) || p < 0 || p > 1) {
    stop("'p' must be a probability (a number between 0 and 1).")
  }

  # Generate random samples from the Binomial distribution
  y <- rbinom(n * iter, size = n, prob = p)

  # Reshape the vector into a matrix with 'n' rows and 'iter' columns
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Compute the mean of each column (sample mean for each iteration)
  w <- colMeans(data)

  # Create a histogram of the sample means without plotting it first
  param <- hist(w, plot = FALSE)

  # Determine the maximum density value for scaling the y-axis
  ymax <- max(param$density) * 1.1  # Add a 10% margin to the maximum density

  # Plot the histogram of sample means with density scaling
  hist(w, freq = FALSE, ylim = c(0, ymax),
       main = paste("Histogram of Sample Means", "\n", "Sample Size = ", n, sep = ""),
       xlab = "Sample Mean", ...)

  # Add a theoretical normal curve for comparison
  curve(dnorm(x, mean = n * p, sd = sqrt(n * p * (1 - p) / n)),
        add = TRUE, col = "red", lty = 2, lwd = 3)
}
