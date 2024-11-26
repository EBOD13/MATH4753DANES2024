#' Calculate Optimal Number of Tickets
#'
#' This function calculates the optimal number of tickets to sell for a flight based on
#' a binomial distribution and its normal approximation, considering the probability of
#' overbooking.
#'
#' @param N Numeric. The number of seats on the flight.
#' @param gamma Numeric. The probability that the plane will be truly overbooked.
#' @param p Numeric. The probability of a "show" (the likelihood that a ticket holder will show up).
#'
#' @importFrom graphics abline curve lines points polygon
#' @importFrom stats dnorm pbinom pnorm
#'
#' @return A list containing:
#' \describe{
#'   \item{nd_discrete}{The optimal number of tickets for the discrete case.}
#'   \item{nc_normal}{The optimal number of tickets for the normal approximation.}
#' }
#' The function also produces two plots for visualizing the objective functions.
#'
#' @export
#'
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
#'
#
# First we need to find the discrete distribution (Binomial) Objective function. We therefore recall that we can use the pbinom() function given by R to find the cumulative distribution function (CDF) - which gives us the probability that N or fewer passengers will show up when we sell n tickets. Hence we have pbinom(N, n, p)

# Second, we need to understand that we can have an over booking. This can happen when, after calculating the CDF, we have a few extra tickets sold. Since we are using a discrete probability, we know that the highest value for such would be 1. Hence, if we take 1 - pbinom(N, n, p), we get the probability that more than N passengers showed up.

# Third, we recall that we are given the probability of overbooking as gamma. With this, if we allow such event to happen, we need to exclude that from our maximum probability. Hence, we now have 1 - gamma, for the maximum allowable probability.
# Lastly, combining them give us the following


# 2. Normal approximation Objective function

# To calculate the Normal approximation Objective function, we first need to normalize our function. That is, we need to find the mean and standard deviation. As we can recall from the working in class we know that the mean of a binomial distribution will be n*p, where n is the number of events and p the probability of success. And the standard deviation is given by: sqrt(n*p*(1-p)).

ntickets <- function(N, gamma, p) {

  # 1. Discrete distribution (Binomial) Objective function
  objective_discrete <- function(n) {
    return(1 - gamma - pbinom(N, size = n, prob = p))  # Binomial CDF
  }

  # 2. Normal approximation Objective function
  objective_normal <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    return(1 - gamma - pnorm(N, mean = mu, sd = sigma))  # Normal CDF
  }

  # 3. Plotting range for n values
  n_values <- seq(N - 10, N + 50, by = 1)  # n values for plotting

  # 4. Objective function values for discrete and normal cases
  discrete_values <- sapply(n_values, objective_discrete)
  normal_values <- sapply(n_values, objective_normal)

  # 5. Find the optimal number of tickets (nd) where objective function crosses 0 (Discrete)
  nd_discrete <- N
  while (objective_discrete(nd_discrete) > 0) {
    nd_discrete <- nd_discrete + 1
  }

  # 6. Find the optimal number of tickets (nc) where objective function crosses 0 (Normal Approx)
  nc_normal <- N
  while (objective_normal(nc_normal) > 0) {
    nc_normal <- nc_normal + 1
  }

  # 7. Plot for Discrete Distribution (Binomial)
  plot(n_values, discrete_values, type = "n",  # Start with an empty plot
       xlab = "Number of tickets (n)", ylab = "Objective",
       main = paste("Discrete Case (Binomial)\nN=", N, "gamma=", gamma, "p=", p),
       xlim = c(N - 10, N + 50), ylim = c(min(discrete_values) - 0.1, max(discrete_values) + 0.1))

  # Add the line connecting the values
  lines(n_values, discrete_values, col = "black")  # Draw the line

  # Add filled circles (closed circles) on the line
  points(n_values, discrete_values, col = "black", pch = 16)  # pch = 16 for closed circles

  abline(h = 0, col = "red", lwd = 2)  # Horizontal line at 0
  abline(v = nd_discrete, col = "red", lwd = 2)  # Vertical line at optimal nd

  # 8. Plot for Continuous Approximation (Normal)
  plot(n_values, normal_values, type = "l", col = "black",
       xlab = "Number of tickets (n)", ylab = "Objective",
       main = paste("Continuous Case (Normal)\nN=", N, "gamma=", gamma, "p=", p),
       xlim = c(N - 10, N + 50), ylim = c(min(normal_values) - 0.1, max(normal_values) + 0.1))

  abline(h = 0, col = "black", lwd = 2)  # Horizontal line at 0
  abline(v = nc_normal, col = "gray", lwd = 2)  # Vertical line at optimal nc

  # Return a list of the optimal ticket numbers
  return(list(nd_discrete = nd_discrete, nc_normal = nc_normal))
}
