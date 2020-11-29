# First drift model

source('./../../gwloggeR/R/fourier.R')

logger.names <- grep('barometer/', gwloggeR.data::enumerate(), value = TRUE)

logger.names <- setdiff(logger.names, 'barometer/BAOL016X_W1666.csv')
logger.names <- setdiff(logger.names, 'barometer/BAOL050X_56819.csv') # high freq manu in range of 80 cmH2O

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

  # Meta data
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]

  data.table::setkey(df, TIMESTAMP_UTC)
  data.table::setattr(df, 'logger.name', logger.name)

  df
}

# from analysis 04
compare <- function(df1, df2) {
  if (nrow(df1) == 0L || nrow(df2) == 0L) return(data.table::data.table())

  diff.df <- df1[J(df2), .('PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE, TIMESTAMP_UTC)][!is.na(PRESSURE_DIFF), ]

  data.table::setkey(diff.df, TIMESTAMP_UTC)

  diff.df
}

ref.compare <- function(logger.name) {
  compare(read.baro(logger.name), read.baro('KNMI_20200312_hourly'))
}

plt.comp <- function(TIMESTAMP_UTC, Y, lm = NULL, front_layer = NULL,
                     xlim = range(TIMESTAMP_UTC), ylim = quantile(Y, c(0.005, 0.995))) {

  force(TIMESTAMP_UTC); force(Y); force(lm)
  force(ylim); force(xlim)

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = Y)) +
    front_layer +
    ggplot2::geom_line(col = 'red', alpha = 0.9)

  if (!is.null(lm))
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes(y = lm$fitted.values), col = 'black', size = 1.5)

  if (!is.null(lm[['btrend']]) && summary(lm)$coefficients['btrend',4] < 0.0001)
    p <- p + ggplot2::geom_vline(xintercept = TIMESTAMP_UTC[which.max(lm$btrend > 0)],
                                 col = 'blue', linetype = 'dotted', size = 1.2)

  p <- p +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot2::ylab('PRESSURE_DIFF') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  p
}

report <- function(logger.name) {

  print(logger.name)

  df.diff <- ref.compare(logger.name)

  if (nrow(df.diff) < 10L) return(invisible(FALSE))

  lm0 <- lm(PRESSURE_DIFF ~ 1, data = df.diff)

  trend <- c(0, cumsum(diff(as.numeric(df.diff$TIMESTAMP_UTC))/3600/24))
  breakpoints <- seq(from = 1, to = nrow(df.diff), by = 10)

  sbasis <- fbasis(timestamps = df.diff$TIMESTAMP_UTC, frequencies = 1/(365.25*3600*24))
  df.seasonal <- data.frame(PRESSURE_DIFF = df.diff$PRESSURE_DIFF,
                            sin = sbasis$`sin(31557600)`,
                            cos = sbasis$`cos(31557600)`)

  lms <- lm(formula = PRESSURE_DIFF ~ . + 1, data = df.seasonal)

  lmt <- lm0
  for (bp in breakpoints) {
    btrend <- c(rep(0, bp - 1), trend[bp:length(trend)] - trend[bp])
    .lm <- lm(PRESSURE_DIFF ~ btrend + 1, data = df.diff)
    .lm[['btrend']] <- btrend
    if (logLik(.lm) > logLik(lmt)) lmt <- .lm
  }

  lmts <- lm0
  for (bp in breakpoints) {
    btrend <- c(rep(0, bp - 1), trend[bp:length(trend)] - trend[bp])
    .lm <- lm(PRESSURE_DIFF ~ btrend + . + 1, data = df.seasonal)
    .lm[['btrend']] <- btrend
    if (logLik(.lm) > logLik(lmts)) lmts <- .lm
  }

  p.comp <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF)

  p.lm0 <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, lm0)

  p.lms <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, lms)

  p.lmt <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, lmt)

  p.lmts <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, lmts)

  # File export ----------------------------------------------------------------

  filename <- sprintf('./drifts/analysis_23/%s.png', tools::file_path_sans_ext(basename(logger.name)))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  layout_matrix <- rbind(c(1, 2),
                         c(3, 4),
                         c(5, 6))

  grob.title <- grid::textGrob(sprintf('%s (#%s differences with KNMI data from %s to %s)',
                                       basename(logger.name), nrow(df.diff),
                                       min(df.diff$TIMESTAMP_UTC), max(df.diff$TIMESTAMP_UTC)),
                               x = 0.05, hjust = 0)

  p.empty <- ggplot2::ggplot() + ggplot2::theme_void()

  local({
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    gridExtra::grid.arrange(p.comp, p.empty,
                            p.lm0, p.lms,
                            p.lmt, p.lmts,
                            layout_matrix = layout_matrix,
                            top = grob.title)
  })

  invisible(TRUE)

}

# report('BAOL009X_78680')
invisible(lapply(logger.names, report))
