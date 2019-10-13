testthat::context('Hydrostatic pressure')

# Detect function test ---------------------------------------------------------

set.seed(2019)
x <- c(-50, rnorm(18), 50, rnorm(19) + 50, -50, rnorm(4), rnorm(45) + 100*0.8^(0:44), 50, rnorm(7))
timestamps <- seq(as.POSIXct('2000-01-01'), length.out = length(x), by = '15 min')
# plot(x)

# saveRDS(detect(x = x, timestamps = timestamps)[order(index, type)], file = './tests/testthat/test-hydropressure.rds')
res.expected <- readRDS('test-hydropressure.rds')
res.expected

testthat::test_that('Hydrostatic pressure detect function finds all the events.', {
  res <- detect(x = x, timestamps = timestamps)[order(index, type)]
  testthat::expect_equal(object = res, expected = res.expected)
})

# Optimization function test ---------------------------------------------------

testthat::test_that("Optimizing 1 point on AO shouldn't have any effect on parameters.", {
  opt <- Optimizer(z = rnorm(1), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)
  res <- opt$optimize()
  testthat::expect_equal(attr(res,"optim")$counts, expected = c('function' = 1, 'gradient' = 1))
})

testthat::test_that("Optimizing without types shoudn't fail.", {
  opt <- Optimizer(z = rnorm(1000), types = NULL, indexes = NULL, mu = 0, sigma2 = 1)
  res <- opt$optimize()
  testthat::expect_length(attr(res, 'par'), n = 0)
})

# Optimizer(z = rnorm(1000), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)$optimize()
# Optimizer(z = rnorm(1000), types = c('AO', 'LS'), indexes = c(1, 3), mu = 0, sigma2 = 1)$optimize()
# Optimizer(z = rnorm(1000), types = c('AO', 'TC', 'LS'), indexes = c(1, 2, 3), mu = 0, sigma2 = 100)$optimize()
