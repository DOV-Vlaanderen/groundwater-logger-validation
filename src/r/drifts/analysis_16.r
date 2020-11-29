# Altitude analysis in continuation with v08. Here we seek out the difference
# between barometric formula height and real height.

source('./drifts/analysis_08.r', encoding = 'UTF-8')
source('./drifts/analysis_15.r', encoding = 'UTF-8')

df.altitude <- data.table::data.table('logger' = names(altitude.m.bf), altitude.m.bf)

location <- function(logger.name) {
  sub('[^/]*/([^_]+).*', '\\1', logger.name)
}

df.altitude[, location := location(logger)]

df.altitude <- merge(
  x = df.altitude,
  y = read.csv('./../../data/meta/inbo/baro_height_dtm.csv',
               dec = ',', sep = ';', na.strings = 'NULL', stringsAsFactors = FALSE)[, c('Peilpunt', 'RASTERVALU')],
  by.x = 'location', by.y = 'Peilpunt', all = TRUE
)

hist(df.altitude$altitude.m.bf - df.altitude$RASTERVALU, breaks = 100, main = '',
     xlab = 'Differences between barometric altitude (m) and TAW')
plot(df.altitude$RASTERVALU, df.altitude$altitude.m.bf, xlab = 'TAW (m)', ylab = 'Barometric altitude (m)')
summary(fit.lm <- lm(formula = altitude.m.bf ~ RASTERVALU, data = df.altitude))
summary(fit.qr <- quantreg::rq(formula = altitude.m.bf ~ RASTERVALU, data = df.altitude))

ggplot2::ggplot(data = df.altitude, mapping = ggplot2::aes(x = RASTERVALU, y = altitude.m.bf)) +
  ggplot2::geom_point() +
  #ggplot2::geom_smooth(method = 'lm', formula = y ~ x, col = 'yellow', size = 1.2, alpha = 0.9) +
  ggplot2::geom_quantile(quantiles = 0.5, formula = y ~ x, col = 'blue', size = 1.2, alpha = 0.9) +
  ggplot2::theme_light() +
  ggplot2::xlab('TAW (m)') +
  ggplot2::ylab('Barometric altitude (m)')

df.altitude[, 'altitude.m.pred' := predict(fit.qr, newdata = data.frame('RASTERVALU' = RASTERVALU))]
df.altitude[, 'altitude.m.diff' := abs(altitude.m.bf - altitude.m.pred)]
df.altitude[, 'N' := vapply(logger, function(name) NROW(df.list[[name]]), 1)]
df.altitude[, 'n.obs' :=  round(x = N, digits = -1L)]
df.altitude[order(altitude.m.diff, decreasing = TRUE), ]

df.altitude <- merge(x = df.altitude, y = stats, by = 'n.obs', all = TRUE)
df.altitude[n.obs > max(stats$n.obs), sd := min(stats$sd)]
df.altitude[, altitude.m.e_err := 2*sd*2 * 100/12]
df.altitude[, altitude.m.rel_err := altitude.m.diff/altitude.m.e_err]
df.altitude[order(altitude.m.rel_err, decreasing = TRUE), ]

ggplot2::ggplot(data = df.altitude, mapping = ggplot2::aes(x = RASTERVALU, y = altitude.m.bf, color = altitude.m.rel_err)) +
  ggplot2::geom_point() +
  ggplot2::scale_color_gradient(low="green", high="red") +
  #ggplot2::geom_smooth(method = 'lm', formula = y ~ x, col = 'yellow', size = 1.2, alpha = 0.9) +
  ggplot2::geom_quantile(quantiles = 0.5, formula = y ~ x, col = 'blue', size = 1.2, alpha = 0.9) +
  ggplot2::theme_light() +
  ggplot2::xlab('TAW (m)') +
  ggplot2::ylab('Barometric altitude (m)')

ggplot2::ggsave('./drifts/analysis_16/baro_altitudes_bf_vs_taw.png', width = 1280/96, height = 720/96, dpi = 96)

write.csv(df.altitude[order(altitude.m.rel_err, decreasing = TRUE), ],
          file = './drifts/analysis_16/baro_altitudes_calculations.csv', row.names = FALSE)
