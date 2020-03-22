# KNMI series analysis similar to v04 en v10.

logger.names <- grep('barodata/', gwloggeR.data::enumerate(), value = TRUE)
logger.names <- c(logger.names, 'KNMI_20200312_hourly')
logger.names <- setdiff(logger.names, 'barodata/BAOL016X_W1666.csv')
logger.names <- setdiff(logger.names, 'barodata/BAOL050X_56819.csv') # high freq manu in range of 80 cmH2O

round_timestamp <- function(ts, scalefactor.sec = 3600*12) {
  as.POSIXct(round(as.numeric(ts)/scalefactor.sec) * scalefactor.sec, origin = '1970-01-01', tz = 'UTC')
}

read.baro <- function(logger.name) {
  df <- gwloggeR.data::read(logger.name)$df
  if (nrow(df) == 0L) return(df)
  df <- df[!is.na(TIMESTAMP_UTC), ]
  df <- df[!is.na(PRESSURE_VALUE), ]
  df <- df[!duplicated(TIMESTAMP_UTC), ]
  df <- df[, .('PRESSURE_VALUE' = mean(PRESSURE_VALUE)),
           by = .('TIMESTAMP_UTC' = round_timestamp(TIMESTAMP_UTC))]
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]
  data.table::setkey(df, TIMESTAMP_UTC)
  structure(df, 'logger.name' = logger.name)
}

# read.baro('barodata/BAOL553X_B_004BA.csv')
data <- sapply(logger.names, read.baro, simplify = FALSE, USE.NAMES = TRUE)

with(data[['KNMI_20200312_hourly']], plot(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, type = 'l'))

compare <- function(df1, df2) {
  if (nrow(df1) == 0L || nrow(df2) == 0L) return(data.table::data.table())

  diff.df <- df1[J(df2), .('PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE, TIMESTAMP_UTC)][!is.na(PRESSURE_DIFF), ]

  diff.df
}

compute <- function(logger.name, make.plot = TRUE) {
  logger.names <- setdiff(logger.names, logger.name)
  logger.df <- data[[logger.name]]

  df.diff <- lapply(logger.names, function(other.name) {
    other.df <- data[[other.name]]
    compare(logger.df, other.df)
  })
  df.diff <- data.table::rbindlist(df.diff, use.names = TRUE, fill = TRUE)

  if (nrow(df.diff) == 0L) return()

  df.quant <- df.diff[
    , .(Q.025 = quantile(PRESSURE_DIFF, 0.025),
        Q.5 = quantile(PRESSURE_DIFF, 0.5),
        Q.975 = quantile(PRESSURE_DIFF, 0.975)),
    by = TIMESTAMP_UTC]

  plt.logger <- ggplot2::ggplot(data = logger.df, mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_VALUE)) +
    ggplot2::geom_line() +
    ggplot2::coord_cartesian(ylim = quantile(logger.df$PRESSURE_VALUE, probs = c(0.005, 0.995)),
                             xlim = range(logger.df$TIMESTAMP_UTC)) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  plt.diff <- ggplot2::ggplot(data = df.diff, mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_DIFF)) +
    ggplot2::geom_point(pch = '.') +
    ggplot2::geom_line(data = df.quant, mapping = ggplot2::aes(y = Q.5), col = 'red', alpha = 0.5) +
    ggplot2::stat_smooth(method = 'gam', formula = y ~ s(x, k = 40), color = 'yellow', se = FALSE) +
    ggplot2::coord_cartesian(ylim = quantile(df.diff$PRESSURE_DIFF, probs = c(0.025, 0.975)),
                             xlim = range(logger.df$TIMESTAMP_UTC)) +
    #ggplot2::scale_x_datetime(date_breaks = '1 year', date_labels = '%Y') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  layout_matrix <- rbind(1,2)
  grob.title <- grid::textGrob(sprintf('%s (#%s vs. #%s observations)',
                                       basename(logger.name), nrow(logger.df), nrow(df.diff)),
                               x = 0.05, hjust = 0)

  filename <- sprintf('./drifts/analysis_11/%s.png', tools::file_path_sans_ext(basename(logger.name)))

  local({
    if (!make.plot) return()
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    print(gridExtra::grid.arrange(plt.logger, plt.diff,
                                  layout_matrix = layout_matrix,
                                  top = grob.title))
  })

  structure(df.diff, 'logger.name' = logger.name)
}

result <- compute('KNMI_20200312_hourly')

# ANALYSIS similar to v10

list.quant <- lapply(list(result), function(df) {
  if (is.null(df)) return(data.table::data.table())
  df.Q <- df[, .(Q.5 = quantile(PRESSURE_DIFF, 0.5), N.GROUP = .N), by = TIMESTAMP_UTC]
  df.Q <- df.Q[N.GROUP >= 10, ]
  wiskers <- boxplot.stats(df.Q[, Q.5])$stats[c(1, 5)]
  df.Q[, Q.5.centered := Q.5 - mean(wiskers)]
  df.Q[, FILE := basename(attr(df, 'logger.name'))]
  df.Q[, N := .N]
  df.Q[, IQR := IQR(Q.5)]
  df.Q[, WISK := wiskers[2L] - wiskers[1L]]
  df.Q[, PERC.95 := quantile(Q.5, 0.975) - quantile(Q.5, 0.025)]
  df.Q[, SD := sd(Q.5)]
  df.Q
})

local(with(list.quant[[1L]], {
  print(ggplot2::ggplot(mapping = ggplot2::aes(x = TIMESTAMP_UTC)) +
          ggplot2::geom_line(mapping = ggplot2::aes(y = Q.5), col = 'red', alpha = 0.5) +
          ggplot2::theme_light() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)))
  plot(x = TIMESTAMP_UTC, y = Q.5, pch = '.')

  par(mfrow = c(1, 2))
  on.exit(par(mfrow = c(1, 1)))
  hist(Q.5, breaks = 50, main = '')
  boxplot(Q.5)
  boxplot.stats(Q.5)$stats
}))

df.quant <- data.table::rbindlist(list.quant) # 424332 observations, 414692 with df.quant[N.GROUP >= 10, ]
df.quant[, LABEL := sprintf('%s (#%i) [%.2f WISK]', FILE, N, WISK)]
df.quant <- df.quant[order(WISK, LABEL), ]
df.quant[, LABEL := factor(LABEL, levels = unique(LABEL))]
df.quant[1L,]
