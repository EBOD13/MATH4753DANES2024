# test-myncurve.R
test_that("myncurve returns correct mu, sigma, and area", {
  result <- MATH4753DANES2024::myncurve(mu = 10, sigma = 5, a = 6)

  # Check if the list contains the correct values
  expect_equal(result$mu, 10)
  expect_equal(result$sigma, 5)
  expect_equal(result$area, round(pnorm(6, mean = 10, sd = 5), 4))
})
