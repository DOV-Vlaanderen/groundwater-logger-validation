# Seasonality analysis

source('./../../gwloggeR/R/graphics.R')
source('./../../gwloggeR/R/fourier.R')
sys.source('./utils/utils.r', envir = (utils <- new.env()))

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

  diff.df
}

#cl <- parallel::makeCluster(5L)
#doParallel::registerDoParallel(cl)
#parallel::stopCluster(cl)

coefs <- function(fit, lambda) {
  coef <- glmnet::coef.glmnet(fit$glmnet.fit, s = lambda)
  coef[coef[, '1'] != 0,,drop = FALSE]
}

coef.periods <- function(coefs) {
  coefs <- coefs[grepl('(sin\\(|cos\\(.*)', rownames(coefs)), , drop = FALSE]
  periods.sec <- sub(pattern = '(sin|cos)\\(([0-9.]+)\\)', replacement = '\\2', rownames(coefs))
  periods.sec <- as.numeric(periods.sec)
  periods.day <- periods.sec/3600/24
  if (length(periods.day) < 2L) return(round(periods.day))
  idx <- cutree(hclust(dist(periods.day)), h = 5)
  round(aggregate(periods.day, by = list(idx), FUN = mean)$x)
}

# # DTFT analysis
#
# dtft(frequency = 1/(365.25*24*3600), x = df.diff$PRESSURE_DIFF, timestamps = df.diff$TIMESTAMP_UTC)
# dtft(frequency = 4/diff(as.numeric(range(df.diff$TIMESTAMP_UTC))), x = df.diff$PRESSURE_DIFF, timestamps = df.diff$TIMESTAMP_UTC)
#
# period <- seq(from = 12*3600*4, to = diff(as.numeric(range(df.diff$TIMESTAMP_UTC))), length.out = 10000)
# fd <- sapply(1/period, FUN = dtft,
#              x = df.diff$PRESSURE_DIFF, timestamps = df.diff$TIMESTAMP_UTC)
# plot(Mod(fd), type = 'l', x = period/3600/24)
# period[which.max(Mod(fd))]/3600/24
# table(diff(df.diff$TIMESTAMP_UTC))

# # FFT analysis
#
# z <- fft(df.diff$PRESSURE_DIFF)
# P <- diff(as.numeric(range(df.diff$TIMESTAMP_UTC)))
# P1 <- P/(length(z) - 1)
# fft.periods <- 1/((0:(length(z) - 1))*P1/P)
# plot(Mod(z), type = 'l',
#      x = fft.periods,
#      xlab = 'days')
# fft.periods[order(Mod(z), decreasing = TRUE)][1:5]
# spectrum(df.diff$PRESSURE_DIFF)
# dft(1L, df.diff$PRESSURE_DIFF)

fit.glmnet <- function(y, x, parallel = FALSE) {

  # alist stores symbols (i.e. doesn't evaluate the arguments.)
  glmnet.args <- alist(
    y = y, x = x,
    alpha = 1,
    parallel = parallel,
    nlambda = 500L,
    foldid = rep(1:10, each = nrow(x)/10, length.out = nrow(x))
  )

  cv.glmnet <- glmnet::cv.glmnet

  fit <- utils$cache$as.file(
    fun = 'cv.glmnet', # this way glmnet stores only "glmnet()" call in fit
    args = glmnet.args,
    prefix = 'glmnet',
    cache.dir = './drifts/analysis_20'
  )

  fit$lambda.2se <- fit$lambda.min + 2*(fit$lambda.1se - fit$lambda.min)

  fit
}

plt.fspec <- function(periods.sec, fd) {
  force(periods.sec); force(fd)
  ggplot2::ggplot(mapping = ggplot2::aes(x = periods.sec/3600/24, y = Mod(fd))) +
    ggplot2::geom_line(col = 'black') +
    ggplot2::geom_vline(xintercept = 365.25, col = 'red') +
    ggplot2::ylab('Intensity') + ggplot2::xlab('Period (days)') +
    #ggplot2::ggtitle('Discrete-Time Fourier Transform') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
}

plt.yearly <- function(TIMESTAMP_UTC, y) {
  force(TIMESTAMP_UTC); force(y)
  ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = data.table::yday(TIMESTAMP_UTC), y = y)) +
    ggplot2::xlab('Day') +
    ggplot2::coord_cartesian(ylim = quantile(y, probs = c(0.001, 0.999)),
                             xlim = c(1, 366)) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
}

periods.label <- function(coefs) {
  p <- coef.periods(coefs)
  has.trend <- any(grepl(pattern = 'trend', x = rownames(coefs), ignore.case = TRUE))
  str <- if (length(p) == 0L) 'Seasonality: /' else sprintf('Seasonality: %s days', paste0(p, collapse = ', '))
  if (has.trend) str <- paste0(str, ' + LinTrend')
  str
}

report <- function(logger.name, ref.logger.names, parallel = FALSE) {

  on.exit(gc(TRUE))

  df.logger <- read.baro(logger.name = logger.name)
  data.table::setkey(df.logger, TIMESTAMP_UTC)

  periods <- seq(from = 2*24*3600,
                 # take minimum a year for the periods
                 to = max(diff(as.numeric(range(df.logger$TIMESTAMP_UTC))), 1.1*365.25*24*3600),
                 length.out = 10000)

  fbase <- data.matrix(fbasis(df.logger$TIMESTAMP_UTC, trend = TRUE, frequencies = 1/periods))

  # Logger analysis ------------------------------------------------------------

  fd.single <- sapply(1/periods, FUN = dtft,
                      x = df.logger$PRESSURE_VALUE - median(df.logger$PRESSURE_VALUE), timestamps = df.logger$TIMESTAMP_UTC)
  #plot(Mod(fd.single), type = 'l', x = periods/3600/24, xlab = 'days')

  p.single.fspec <- plt.fspec(periods.sec = periods, fd = fd.single)

  p.single.yearly <- plt.yearly(TIMESTAMP_UTC = df.logger$TIMESTAMP_UTC, y = df.logger$PRESSURE_VALUE)

  fit.single <- fit.glmnet(y = df.logger$PRESSURE_VALUE, x = fbase, parallel = parallel)
  #plot(fit.single)
  #coef.periods(coefs(fit.single, lambda = fit.single$lambda.min))

  df.logger[, PRESSURE_VALUE_GLMNET_MIN := glmnet::predict.glmnet(fit.single$glmnet.fit, newx = fbase, s = fit.single$lambda.min)]
  df.logger[, PRESSURE_VALUE_GLMNET_1SE := glmnet::predict.glmnet(fit.single$glmnet.fit, newx = fbase, s = fit.single$lambda.1se)]

  p.single <- ggplot2::ggplot(data = df.logger, mapping = ggplot2::aes(x = TIMESTAMP_UTC)) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = PRESSURE_VALUE), col = 'black') +
    ggplot2::geom_line(mapping = ggplot2::aes(y = df.logger$PRESSURE_VALUE_GLMNET_MIN), col = 'blue', size = 1.3) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = df.logger$PRESSURE_VALUE_GLMNET_1SE), col = 'green', size = 1.3) +
    ggplot2::coord_cartesian(ylim = quantile(with(df.logger, PRESSURE_VALUE), probs = c(0.001, 0.999)),
                             xlim = range(df.logger$TIMESTAMP_UTC)) +
    ggplot2::annotate(
      "label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4, col = 'green', hjust = 0, vjust = 1, fill = 'grey', label.size = NA,
      label = periods.label(coefs(fit.single, lambda = fit.single$lambda.1se))
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  # Multi comparison -----------------------------------------------------------

  df.diff <- lapply(setdiff(basename(ref.logger.names), basename(logger.name)), function(other.name) {
    df.other <- read.baro(other.name)
    compare(df.logger, df.other)
  })
  df.diff <- data.table::rbindlist(df.diff, use.names = TRUE, fill = TRUE)

  df.multi <- df.diff[
    , .(Q.025 = quantile(PRESSURE_DIFF, 0.025),
        Q.5 = quantile(PRESSURE_DIFF, 0.5),
        Q.975 = quantile(PRESSURE_DIFF, 0.975)),
    by = TIMESTAMP_UTC]

  fit.multi <- fit.glmnet(y = df.multi$Q.5, x = fbase, parallel = parallel)
  #plot(fit.multi)
  #coefs(fit.multi, lambda = fit.multi$lambda.1se)
  #coef.periods(coefs(fit.multi, lambda = fit.multi$lambda.1se))

  df.multi[, Q.5_GLMNET_MIN := glmnet::predict.glmnet(fit.multi$glmnet.fit, newx = fbase, s = fit.multi$lambda.min)]
  df.multi[, Q.5_GLMNET_1SE := glmnet::predict.glmnet(fit.multi$glmnet.fit, newx = fbase, s = fit.multi$lambda.1se)]

  p.multi <- ggplot2::ggplot(data = df.diff, mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_DIFF)) +
    ggplot2::geom_point(pch = '.') +
    ggplot2::geom_line(data = df.multi, mapping = ggplot2::aes(y = Q.5), col = 'red', alpha = 0.9) +
    ggplot2::geom_line(data = df.multi, mapping = ggplot2::aes(y = Q.5_GLMNET_MIN), col = 'blue', size = 1.3) +
    ggplot2::geom_line(data = df.multi, mapping = ggplot2::aes(y = Q.5_GLMNET_1SE), col = 'green', size = 1.3) +
    ggplot2::coord_cartesian(ylim = quantile(df.multi$Q.5, probs = c(0.005, 0.995)),
                             xlim = range(df.logger$TIMESTAMP_UTC)) +
    ggplot2::annotate(
      "label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4, col = 'green', hjust = 0, vjust = 1, fill = 'grey', label.size = NA,
      label = periods.label(coefs(fit.multi, lambda = fit.multi$lambda.1se))
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  fd.multi <- sapply(1/periods, FUN = dtft,
                     x = df.multi$Q.5 - median(df.multi$Q.5), timestamps = df.multi$TIMESTAMP_UTC)
  #plot(Mod(fd.multi), type = 'l', x = periods/3600/24, xlab = 'days')

  p.multi.fspec <- plt.fspec(periods.sec = periods, fd = fd.multi)

  p.multi.yearly <- plt.yearly(TIMESTAMP_UTC = df.multi$TIMESTAMP_UTC, y = df.multi$Q.5)

  # Reference comparison -------------------------------------------------------

  df.ref <- compare(df.logger, read.baro('KNMI_20200312_hourly'))
  #plot(df.ref$PRESSURE_DIFF, type = 'l', x = df.ref$TIMESTAMP_UTC)

  fit.ref <- fit.glmnet(y = df.ref$PRESSURE_DIFF, x = fbase, parallel = parallel)
  #plot(fit.ref)
  #coef.periods(coefs(fit.ref, lambda = fit.ref$lambda.1se))

  df.ref[, PRESSURE_DIFF_GLMNET_MIN := glmnet::predict.glmnet(fit.ref$glmnet.fit, newx = fbase, s = fit.ref$lambda.min)]
  df.ref[, PRESSURE_DIFF_GLMNET_1SE := glmnet::predict.glmnet(fit.ref$glmnet.fit, newx = fbase, s = fit.ref$lambda.1se)]

  p.ref <- ggplot2::ggplot(data = df.ref, mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_DIFF)) +
    ggplot2::geom_point(pch = '.') +
    ggplot2::geom_line(col = 'red', alpha = 0.8) +
    ggplot2::geom_line(data = df.ref, mapping = ggplot2::aes(y = PRESSURE_DIFF_GLMNET_MIN), col = 'blue', size = 1.3) +
    ggplot2::geom_line(data = df.ref, mapping = ggplot2::aes(y = PRESSURE_DIFF_GLMNET_1SE), col = 'green', size = 1.3) +
    ggplot2::coord_cartesian(ylim = quantile(df.ref$PRESSURE_DIFF, probs = c(0.005, 0.995)),
                             xlim = range(df.logger$TIMESTAMP_UTC)) +
    ggplot2::annotate(
      "label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4, col = 'green', hjust = 0, vjust = 1, fill = 'grey', label.size = NA,
      label = periods.label(coefs(fit.ref, lambda = fit.ref$lambda.1se))
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  fd.ref <- sapply(1/periods, FUN = dtft,
                   x = df.ref$PRESSURE_DIFF - median(df.ref$PRESSURE_DIFF), timestamps = df.ref$TIMESTAMP_UTC)
  #plot(Mod(fd.ref), type = 'l', x = periods/3600/24, xlab = 'days')

  p.ref.fspec <- plt.fspec(periods.sec = periods, fd = fd.ref)

  p.ref.yearly <- plt.yearly(TIMESTAMP_UTC = df.ref$TIMESTAMP_UTC, y = df.ref$PRESSURE_DIFF)

  # File export ----------------------------------------------------------------

  filename <- sprintf('./drifts/analysis_20/%s.png', tools::file_path_sans_ext(basename(logger.name)))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  layout_matrix <- rbind(c(1, 1, 1, 4, 5),
                         c(2, 2, 2, 6, 7),
                         c(3, 3, 3, 8, 9))

  grob.title <- grid::textGrob(sprintf('%s (#%s observations from %s to %s)',
                                       basename(logger.name), nrow(df.logger),
                                       min(df.logger$TIMESTAMP_UTC), max(df.logger$TIMESTAMP_UTC)),
                               x = 0.05, hjust = 0)

  local({
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    print(gridExtra::grid.arrange(p.single, p.multi, p.ref,
                                  p.single.fspec, p.single.yearly,
                                  p.multi.fspec, p.multi.yearly,
                                  p.ref.fspec, p.ref.yearly,
                                  layout_matrix = layout_matrix,
                                  top = grob.title))
  })

  invisible(TRUE)
}

# report('BAOL828X_P2_15705.csv', ref.logger.names = basename(logger.names), parallel = TRUE)
# report('BAOL040X_555351', ref.logger.names = basename(logger.names), parallel = TRUE)

parallel::setDefaultCluster(parallel::makeCluster(spec = 4L))
parallel::clusterExport(varlist = ls())
results <- parallel::parSapplyLB(X = logger.names, FUN = report, chunk.size = 1,
                                 ref.logger.names = logger.names)
parallel::stopCluster(cl = parallel::getDefaultCluster())
