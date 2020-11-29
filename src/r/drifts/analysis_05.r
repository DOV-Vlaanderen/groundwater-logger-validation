# Effect of temperature on air pressure.

logger.names <- grep('barometer/', gwloggeR.data::enumerate(), value = TRUE)

logger.names <- setdiff(logger.names, 'barometer/BAOL016X_W1666.csv')
logger.names <- setdiff(logger.names, 'barometer/BAOL050X_56819.csv') # high freq manu in range of 80 cmH2O

df <- lapply(logger.names, function(name) gwloggeR.data::read(name)$df)
df <- data.table::rbindlist(df, use.names = TRUE, fill = TRUE)
df <- df[!is.na(TEMPERATURE_VALUE), ]
df <- df[!is.na(PRESSURE_VALUE), ]

with(df, hist(TEMPERATURE_VALUE, breaks = 1000, main = '',
              xlim = quantile(TEMPERATURE_VALUE, probs = c(0.0001, 0.9999))))
with(df, hist(PRESSURE_VALUE, breaks = 1000, main = '',
              xlim = quantile(PRESSURE_VALUE, probs = c(0.0001, 0.9999))))

range(df$TEMPERATURE_VALUE)
range(df$PRESSURE_VALUE)

df.quant <- df[
  , .(Q.025 = quantile(PRESSURE_VALUE, 0.025),
      Q.5 = quantile(PRESSURE_VALUE, 0.5),
      Q.975 = quantile(PRESSURE_VALUE, 0.975),
      N = .N),
  by = .(TEMPERATURE_VALUE = round(TEMPERATURE_VALUE))][N > 200, ]

summary((fit.lm <- lm(data = df, formula = PRESSURE_VALUE ~ TEMPERATURE_VALUE)))
summary((fit.rq <- quantreg::rq(data = df, formula = PRESSURE_VALUE ~ TEMPERATURE_VALUE)))
# fit <- bayesQR::bayesQR(data = df, formula = PRESSURE_VALUE ~ TEMPERATURE_VALUE, ndraw = 10)

ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = PRESSURE_VALUE)) +
  ggplot2::geom_point(pch = '.') +
  ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = Q.025), col = 'red', size = 1.2, alpha = 0.4) +
  ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = Q.5), col = 'red', size = 1.2, alpha = 0.9) +
  ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = Q.975), col = 'red', size = 1.2, alpha = 0.4) +
  ggplot2::coord_cartesian(ylim = quantile(df$PRESSURE_VALUE, probs = c(0.0001, 0.9999)),
                           xlim = quantile(df$TEMPERATURE_VALUE, probs = c(0.0001, 0.9999))) +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_05/air_pressure_temp_simple.png', width = 1280/96, height = 720/96, dpi = 96)

ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = PRESSURE_VALUE)) +
  ggplot2::geom_point(pch = '.') +
  ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = Q.025), col = 'red', size = 1.2, alpha = 0.4) +
  ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = Q.5), col = 'red', size = 1.2, alpha = 0.9) +
  ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(x = TEMPERATURE_VALUE, y = Q.975), col = 'red', size = 1.2, alpha = 0.4) +
  ggplot2::geom_smooth(method = 'lm', formula = y ~ x, col = 'yellow', size = 1.2, alpha = 0.9) +
  ggplot2::geom_quantile(quantiles = 0.5, formula = y ~ x, col = 'blue', size = 1.2, alpha = 0.9) +
  ggplot2::coord_cartesian(ylim = quantile(df$PRESSURE_VALUE, probs = c(0.0001, 0.9999)),
                           xlim = quantile(df$TEMPERATURE_VALUE, probs = c(0.0001, 0.9999))) +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_05/air_pressure_temp_regressed.png', width = 1280/96, height = 720/96, dpi = 96)
