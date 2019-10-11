testthat::context('Indicator function')

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


testthat::test_that('Indicator function failes on unknown type.', {
  testthat::expect_error(indicator('AA', 5, 5))
})