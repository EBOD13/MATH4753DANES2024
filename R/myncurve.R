# R/myncurve.R

# Declare 'x' as a global variable to avoid R CMD check NOTE
if (getRversion() >= "2.15.1") utils::globalVariables(c("x"))

#' myncurve function
#'
#' This function displays a normal distribution curve, shades the area between the curve and the x-axis from -infinity to x = a, and returns the area (probability P(X ≤ a)).
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a The upper limit for shading the area under the curve (x = a).
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @return A list containing:
#' \describe{
#'   \item{mu}{The mean of the distribution.}
#'   \item{sigma}{The standard deviation of the distribution.}
#'   \item{area}{The calculated probability P(X ≤ a), rounded to four decimal places.}
#' }
#'
#' @export
#'
#' @examples
#' myncurve(mu = 0, sigma = 1, a = 1.96)
myncurve <- function(mu, sigma, a) {

  # Only plot if in an interactive session
  if (interactive()) {
    # Plot the normal curve
    curve(dnorm(x, mean = mu, sd = sigma),
          xlim = c(mu - 3 * sigma, mu + 3 * sigma),
          ylab = "Density",
          xlab = "X",
          main = paste("Normal Distribution: mu =", mu, ", sigma =", sigma))

    # Shade the area from -infinity to x = a
    xcurve <- seq(mu - 3 * sigma, a, length.out = 1000)  # x values from mu - 3*sigma to a
    ycurve <- dnorm(xcurve, mean = mu, sd = sigma)      # y values corresponding to x values

    # Fill the polygon with the given vertices
    polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "lightblue")
  }

  # Calculate the area (probability)
  area <- pnorm(a, mean = mu, sd = sigma)

  # Return the result as a list
  result <- list(mu = mu, sigma = sigma, area = round(area, 4))  # Round to 4 decimal places
  return(result)
}
