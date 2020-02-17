# Analysis of differences between one barometer and many others in its neighbourhood.
# This one is the same as v02, but uses better matching of timestamps.

logger.names <- grep('barodata/', gwloggeR.data::enumerate(), value = TRUE)

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
  data.table::setkey(df, TIMESTAMP_UTC)
  structure(df, 'logger.name' = logger.name)
}

# read.baro('barodata/BAOL553X_B_004BA.csv')

compare <- function(df1, df2) {
  if (nrow(df1) == 0L || nrow(df2) == 0L) return(data.table::data.table())

  diff.df <- df1[J(df2), .('PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE, TIMESTAMP_UTC)][!is.na(PRESSURE_DIFF), ]

  diff.df
}


compute <- function(logger.name) {
  logger.names <- setdiff(logger.names, logger.name)
  logger.df <- read.baro(logger.name)

  df.diff <- lapply(logger.names, function(other.name) {
    other.df <- read.baro(other.name)
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

  filename <- sprintf('./drifts/analysis_04/%s.png', tools::file_path_sans_ext(basename(logger.name)))

  local({
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    print(gridExtra::grid.arrange(plt.logger, plt.diff,
                                  layout_matrix = layout_matrix,
                                  top = grob.title))
  })

}

# compute(logger.names[1])
# lapply(logger.names, compute)

parallel::setDefaultCluster(parallel::makeCluster(spec = 7L))
parallel::clusterExport(varlist = c('logger.names', 'round_timestamp', 'read.baro', 'compare'))
invisible(parallel::clusterApplyLB(x = logger.names, fun = compute))
parallel::stopCluster(cl = parallel::getDefaultCluster())

