samples <- sapply(as.character(seq(5, 1*60*24*4, by = 5)*60), function(interval.sec) {
  gwloggeR:::apriori.hydropressure.difference.samples(as.numeric(interval.sec))
}, simplify = FALSE, USE.NAMES = TRUE)

df <- data.table::rbindlist(lapply(names(samples), function(interval.sec) {
  data.table::data.table('TIMEDIFF' = as.numeric(interval.sec),
                         'VALUE' = unlist(unname(samples[[interval.sec]])))
}), use.names = TRUE, idcol = FALSE)
data.table::setkey(df, TIMEDIFF)

ggplot2::ggplot(data = df[, .(MEDIAN = median(abs(VALUE)),
                              N = length(VALUE),
                              Q.025 = quantile(abs(VALUE), 0.025),
                              Q.975 = quantile(abs(VALUE), 0.975),
                              Q.99 = quantile(abs(VALUE), 0.99)),
                          by = TIMEDIFF/60/60],
                mapping = ggplot2::aes(x = TIMEDIFF)) +
  ggplot2::geom_line(mapping = ggplot2::aes(y = MEDIAN)) +
  ggplot2::geom_line(mapping = ggplot2::aes(y = Q.975), col = 'red') +
  ggplot2::xlab('TIMEDIFF (hour)') + ggplot2::ylab('ABSOLUTE VALUEDIFF (cmH2O)') +
  ggplot2::ggtitle('Median (black) and 0.975 quantile (red) of absolute value differences of aprior hydrostatic pressure data in function of time.') +
  ggplot2::scale_x_continuous(breaks = seq(0, 200, by = 24.833/2)) + # lunar day: 24h50
  ggplot2::theme_minimal()

ggplot2::ggsave('./apriori/apriori_hydrostatic_pressure_quantiles.png', width = 16, height = 9)

df.ecdf <- df[, .(ECDF = list(ecdf(VALUE))), by = TIMEDIFF]
data.table::setkey(df.ecdf, TIMEDIFF)

# x = TIMEDIFF, y = VALUE
cumulative.density <- function(x, y) {
  data.table::rbindlist(Vectorize(function(x, y) {
    data.frame('CDF' = df.ecdf[TIMEDIFF == x, ECDF][[1]](y), x, y)
  }, vectorize.args = 'x', SIMPLIFY = FALSE)(x, y))
}

df.grad <- cumulative.density(x = df.ecdf[, TIMEDIFF], y = seq(-150, 150, length.out = 1000))

ggplot2::ggplot(data = df.grad, mapping = ggplot2::aes(x = x/60/60, y = y)) +
  ggplot2::geom_raster(mapping = ggplot2::aes(fill = CDF), interpolate = TRUE, alpha = 0.8) +
  ggplot2::scale_fill_gradient2(low = 'white', mid = 'red', high = 'white', midpoint = 0.5) +
  ggplot2::xlab('TIMEDIFF (hour)') + ggplot2::ylab('VALUEDIFF (cmH2O)') +
  ggplot2::ggtitle('Cumulative density of aprior hydrostatic pressure data in function of time.') +
  ggplot2::scale_x_continuous(breaks = seq(0, 200, by = 24.833/2)) + # lunar day: 24h50
  ggplot2::theme_minimal()

ggplot2::ggsave('./apriori/apriori_hydrostatic_pressure_cumulative_density.png', width = 16, height = 9)
