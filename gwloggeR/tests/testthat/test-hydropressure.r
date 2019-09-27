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
