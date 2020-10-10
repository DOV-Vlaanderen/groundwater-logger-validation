# Best fixed AR(1) model for drift detection
# Rest is taken over from analysis v23 and v26

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

coef.pvals <- function(M) {
  (1-pnorm(abs(M$coef[colnames(M$var.coef)])/sqrt(diag(M$var.coef))))*2
}

plt.comp <- function(TIMESTAMP_UTC, Y, M = NULL, significant = NULL, front_layer = NULL,
                     xlim = range(TIMESTAMP_UTC), ylim = quantile(Y, c(0.005, 0.995))) {

  force(TIMESTAMP_UTC); force(Y); force(M)
  force(ylim); force(xlim)

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = Y)) +
    front_layer +
    ggplot2::geom_line(col = 'red', alpha = 0.9)

  if (!is.null(M))
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes(y = fitted(M)), col = 'black', size = 1.5)

  if (!is.null(significant)) {
    p <- p + ggplot2::annotate(
      "label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4, col = if (significant) 'green' else 'red',
      hjust = 0, vjust = 1, fill = 'grey', label.size = NA,
      label = if (significant) 'SIGNIFINCANT' else 'NOT SIGNIFICANT'
    )
  }

  if (!is.null(M[['btrend']]) && coef.pvals(M)['btrend'] < 0.0001)
    p <- p + ggplot2::geom_vline(xintercept = TIMESTAMP_UTC[which.max(M$btrend > 0)],
                                 col = 'blue', linetype = 'dotted', size = 1.2)

  p <- p +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot2::ylab('PRESSURE_DIFF') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())

  p
}

report <- function(logger.name) {
  require(forecast)

  environment(plt.comp) <- environment() # fitted(Arima) then can find the source data.

  print(logger.name)

  df.diff <- ref.compare(logger.name)

  if (nrow(df.diff) < 10L) return(invisible(FALSE))

  M <- arima(x = df.diff$PRESSURE_DIFF, order = c(0, 0, 0))
  df.diff[, M_pred := fitted(M)]

  M.AR <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0), transform.pars = FALSE, fixed = c(0.90, NA))
  df.diff[, M.AR.pred := fitted(M.AR)]

  trend <- c(0, cumsum(diff(as.numeric(df.diff$TIMESTAMP_UTC))/3600/24))
  breakpoints <- seq(from = 1, to = nrow(df.diff) - 1, by = 10)

  M.ARD <- M.AR
  for (bp in breakpoints) {
    btrend <- c(rep(0, bp), trend[-(1:bp)] - (bp/2 - 0.5))
    .M <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0), xreg = btrend, transform.pars = FALSE, fixed = c(0.90, NA, NA))
    .M[['btrend']] <- btrend
    if (logLik(.M) > logLik(M.ARD)) M.ARD <- .M
  }
  df.diff[, M.ARD.pred := fitted(M.ARD)]


  sbasis <- fbasis(timestamps = df.diff$TIMESTAMP_UTC, frequencies = 1/(365.25*3600*24))

  M.ARS <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0), xreg = sbasis, transform.pars = FALSE, fixed = c(0.90, NA, NA, NA))

  M.ARDS <- M.ARS
  for (bp in breakpoints) {
    btrend <- c(rep(0, bp), trend[-(1:bp)] - (bp/2 - 0.5))
    .M <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0), xreg = cbind(sbasis, btrend), transform.pars = FALSE, fixed = c(0.90, NA, NA, NA, NA))
    .M[['btrend']] <- btrend
    if (logLik(.M) > logLik(M.ARDS)) M.ARDS <- .M
  }

  M.AUTO <- forecast::auto.arima(df.diff$PRESSURE_DIFF, trace = TRUE,
                                 stationary = TRUE, ic = 'aicc',
                                 xreg = data.matrix(cbind(sbasis, 'btrend' = M.ARDS[['btrend']])))
  M.AUTO[['btrend']] <- M.ARDS[['btrend']]

  df.diff[, TIMESTAMP_UTC_DIFF_HOURS := c(NA, round(diff(as.numeric(TIMESTAMP_UTC))/3600))]

  p.comp <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF)

  p.M <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M)

  p.M.AR <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.AR)

  p.M.ARS <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.ARS)

  p.M.ARD <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.ARD)

  p.M.ARDS <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.ARDS)

  p.M.AUTO <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.AUTO)

  p.ts.diff <- ggplot2::ggplot(data = df.diff[-1, ], mapping = ggplot2::aes(
    x = TIMESTAMP_UTC,
    y = TIMESTAMP_UTC_DIFF_HOURS)) +
    ggplot2::scale_y_continuous(
      trans = scales::trans_new(name = 'log12',
                                transform = function(x) logb(x = x, base = 12),
                                inverse = function(x) 12^x)) +
    ggplot2::geom_point(pch = '-', size = 8) +
    ggplot2::ylab('PRESSURE_DIFF') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())

  # File export ----------------------------------------------------------------

  filename <- sprintf('./drifts/analysis_28/%s.png', tools::file_path_sans_ext(basename(logger.name)))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  layout_matrix <- rbind(c(1, 2),
                         c(3, 4),
                         c(5, 6))

  grob.title <- grid::textGrob(sprintf('%s (#%s differences with KNMI data from %s to %s) -> %s | Fixed AR(1) = 0.9',
                                       basename(logger.name), nrow(df.diff),
                                       min(df.diff$TIMESTAMP_UTC), max(df.diff$TIMESTAMP_UTC),
                                       forecast:::arima.string(M.AUTO)),
                               x = 0.05, hjust = 0)

  p.empty <- ggplot2::ggplot() + ggplot2::theme_void()

  local({
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    gridExtra::grid.arrange(p.M.AUTO, p.ts.diff,
                            p.M.AR, p.M.ARS,
                            p.M.ARD, p.M.ARDS,
                            layout_matrix = layout_matrix,
                            top = grob.title)
  })

  invisible(TRUE)

}

# report('BAOL009X_78680')
invisible(lapply(logger.names, report))
