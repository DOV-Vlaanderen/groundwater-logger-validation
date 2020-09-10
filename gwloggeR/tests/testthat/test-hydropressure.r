testthat::context('Hydrostatic pressure')

# Indicator function test ------------------------------------------------------

testthat::test_that('Indicator function for last point.', {
  # Last point can never be detected as an AO, only LS
  testthat::expect_equal(object = indicator('AO', 1, 1), expected = 0)
  testthat::expect_equal(object = indicator('LS', 1, 1), expected = 1)

  testthat::expect_equal(object = indicator('TC', 1, 1), expected = 1)
})


testthat::test_that('Indicator function for AO, LS and TC events produces correct results.', {
  testthat::expect_equal(object = indicator('AO', 2, 5), expected = c(0, 1,-1, 0, 0))
  testthat::expect_equal(object = indicator('LS', 1, 5), expected = c(1, 0, 0, 0, 0))
  testthat::expect_equal(object = indicator('TC', 5, 5), expected = c(0, 0, 0, 0, 1))
  testthat::expect_equal(object = indicator('TC', 4, 5), expected = c(0, 0, 0, 1, 1))
  testthat::expect_equal(object = indicator('TC', 1, 5), expected = c(1, 1, 1, 1, 1))
})


testthat::test_that('Indicator function fails on unknown type.', {
  testthat::expect_error(indicator('AA', 5, 5))
})

# Decay function test ------------------------------------------------------

testthat::test_that('Decay function works correctly.', {
  testthat::expect_equal(object = round(decay(2, 0.7, 5), 2), c(0.00,  1.00, -0.30, -0.21, -0.15))
  testthat::expect_equal(object = round(decay(1, 0.7, 2), 2), c(1.00, -0.30))
  testthat::expect_equal(object = round(decay(1, 0.7, 1), 2), 1)
})

# Detect function test ---------------------------------------------------------

set.seed(2019)
x <- c(-50, rnorm(18), 50, rnorm(19) + 50, -50, rnorm(4), rnorm(45) + 100*0.8^(0:44), 50, rnorm(7))
timestamps <- seq(as.POSIXct('2000-01-01'), length.out = length(x), by = '15 min')
# plot(x)

testthat::test_that('Hydrostatic pressure detect function finds all the events.', {
  res <- detect(x = x, timestamps = timestamps)[order(index, type)]
  testthat::expect_known_value(object = res, file = './test-hydropressure-detect.rds')
})

testthat::test_that('Hydrostatic pressure LS is detected.', {
  res.expected <- rep(FALSE, length(x))
  res.expected[c(2, 20, 40)] <- TRUE
  res <- detect_levelshifts(x = x, timestamps = timestamps,
                            apriori = apriori('hydrostatic pressure'))
  testthat::expect_equal(res, res.expected)
})

testthat::test_that('Hydrostatic pressure TC is detected.', {
  res.expected <- rep(FALSE, length(x))
  res.expected[45:58] <- TRUE
  res <- detect_temporalchanges(x = x, timestamps = timestamps,
                                apriori = apriori('hydrostatic pressure'))
  testthat::expect_equal(res, res.expected)
})


# Optimization function test ---------------------------------------------------

testthat::test_that("Optimizing 1 point on AO shouldn't have any effect on parameters.", {
  opt <- Optimizer(dx = rnorm(1), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)
  res <- opt$optimize()
  testthat::expect_equal(attr(res,"optim")$counts, expected = c('function' = 1, 'gradient' = 1))
})

testthat::test_that("Optimizing without types shoudn't fail.", {
  opt <- Optimizer(dx = rnorm(1000), types = NULL, indexes = NULL,
                   mu = rep(0, 1000), sigma2 = rep(1, 1000))
  res <- opt$optimize()
  testthat::expect_length(attr(res, 'par'), n = 0)
})

# Optimizer(dx = rnorm(1000), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)$optimize()
# Optimizer(dx = rnorm(1000), types = c('AO', 'LS'), indexes = c(1, 3), mu = 0, sigma2 = 1)$optimize()
# Optimizer(dx = rnorm(1000), types = c('AO', 'TC', 'LS'), indexes = c(1, 2, 3), mu = 0, sigma2 = 100)$optimize()
