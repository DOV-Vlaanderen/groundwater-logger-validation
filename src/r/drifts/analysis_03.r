# Seasonality analysis

loggers <- grep('barodata/', gwloggeR.data::enumerate('inbo'), value = TRUE)

df <- lapply(loggers, function(logger) {
  gwloggeR.data::read(logger)$df
})
df <- data.table::rbindlist(df, use.names = TRUE, fill = TRUE)
df <- df[!is.na(TIMESTAMP_UTC), ][!is.na(PRESSURE_VALUE), ]
data.table::setkey(df, TIMESTAMP_UTC)
df[, TIME := as.POSIXct(format(TIMESTAMP_UTC, '2000-01-01 %H:00:00'))]
df[, TIME_YEAR := as.POSIXct(format(TIMESTAMP_UTC, '2000-%m-%d %H:00:00'))]

df.time <- df[, .(Q.025 = quantile(PRESSURE_VALUE, 0.025),
                  Q.5 = quantile(PRESSURE_VALUE, 0.5),
                  Q.975 = quantile(PRESSURE_VALUE, 0.975),
                  N = .N),
              by = TIME]

df.time_year <- df[, .(Q.025 = quantile(PRESSURE_VALUE, 0.025),
                       Q.5 = quantile(PRESSURE_VALUE, 0.5),
                       Q.975 = quantile(PRESSURE_VALUE, 0.975),
                       N = .N),
                   by = TIME_YEAR]

ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = TIME, y = PRESSURE_VALUE)) +
  #ggplot2::geom_point(pch = '.', alpha = 0.5) +
  ggplot2::geom_line(data = df.time, mapping = ggplot2::aes(y = Q.5), col = 'red', alpha = 0.8, size = 1.5) +
  ggplot2::coord_cartesian(ylim = quantile(df$PRESSURE_VALUE, probs = c(0.025, 0.975))) +
  ggplot2::scale_x_datetime(date_breaks = '1 hour', date_labels = '%H') +
  ggplot2::ggtitle(sprintf('Air pressure seasonality (day): %i observations, truncated to hour', nrow(df))) +
  ggplot2::xlab('HOUR') +
  ggplot2::theme_light()

ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = TIME_YEAR, y = PRESSURE_VALUE)) +
  ggplot2::geom_point(pch = '.', alpha = 0.5) +
  ggplot2::geom_line(data = df.time_year, mapping = ggplot2::aes(y = Q.025), col = 'red', alpha = 0.4, size = 1.2) +
  ggplot2::geom_line(data = df.time_year, mapping = ggplot2::aes(y = Q.5), col = 'red', alpha = 0.8, size = 1.5) +
  ggplot2::geom_line(data = df.time_year, mapping = ggplot2::aes(y = Q.975), col = 'red', alpha = 0.4, size = 1.2) +
  ggplot2::coord_cartesian(ylim = quantile(df$PRESSURE_VALUE, probs = c(0.025, 0.975))) +
  ggplot2::scale_x_datetime(date_breaks = '1 month', date_labels = '%m') +
  ggplot2::ggtitle(sprintf('Air pressure seasonality (year): %i observations, truncated to hour', nrow(df))) +
  ggplot2::xlab('MONTH') +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_03/air_pressure_seasonality_year.png', width = 1280, height = 720, dpi = 1, limitsize = FALSE)
