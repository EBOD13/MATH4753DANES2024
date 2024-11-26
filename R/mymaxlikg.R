#' Maximum Likelihood Estimation Function
#'
#' Computes the maximum likelihood estimate (MLE) for a given log-likelihood function
#' over a range of parameter values and plots the corresponding likelihood function.
#'
#' @param lfun A function that computes the log-likelihood for a given parameter value.
#' @param theta A numeric vector of parameter values to evaluate the likelihood.
#'
#' @return The parameter value corresponding to the maximum likelihood estimate (MLE).
#' @export
#'
#' @importFrom graphics plot abline axis
#'
#' @examples
#' # Define a sample log-likelihood function
#' logbin2 <- function(theta) {
#'   n <- 10  # Example: number of trials
#'   x <- 4   # Example: observed successes
#'   return(x * log(theta) + (n - x) * log(1 - theta)) # Binomial log-likelihood
#' }
#'
#' # Run the MLE function
#' theta_values <- seq(0.01, 0.99, by = 0.01)  # Parameter values for theta
#' max_lik_estimate <- mymaxlikg(lfun = logbin2, theta = theta_values)
#' print(max_lik_estimate)
mymaxlikg <- function(lfun, theta) {
  # Input validation
  if (!is.function(lfun)) {
    stop("'lfun' must be a callable function.")
  }
  if (!is.numeric(theta) || length(theta) == 0) {
    stop("'theta' must be a non-empty numeric vector.")
  }

  # Compute log-likelihood values for each theta
  z <- sapply(theta, lfun)

  # Find the index of the maximum likelihood
  zmax <- which.max(z)

  # Plot the likelihood function
  plot(theta, exp(z), type = "l", main = "Likelihood Function",
       xlab = "Parameter (theta)", ylab = "Likelihood", col = "black")
  abline(v = theta[zmax], col = "blue")  # Vertical line at max likelihood
  axis(3, at = theta[zmax], labels = round(theta[zmax], 4))  # Add tick for MLE

  # Return the parameter value corresponding to max likelihood
  return(theta[zmax])
}
