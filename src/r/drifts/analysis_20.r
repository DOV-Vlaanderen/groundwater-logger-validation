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
  idx <- cutree(hclust(dist(periods.day)), h = 10)
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

plt.fspec <- function(periods.sec, intensity) {
  force(periods.sec); force(intensity)
  ggplot2::ggplot(mapping = ggplot2::aes(x = periods.sec/3600/24, y = intensity)) +
    ggplot2::geom_line(col = 'black') +
    ggplot2::geom_vline(xintercept = 365.25, col = 'red') +
    #ggplot2::ylab('Intensity') +
    ggplot2::xlab('Period (days)') +
    #ggplot2::ggtitle('Discrete-Time Fourier Transform') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
}

plt.yearly <- function(TIMESTAMP_UTC, y, ylim = quantile(y, probs = c(0.001, 0.999))) {
  force(TIMESTAMP_UTC); force(y)
  ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = data.table::yday(TIMESTAMP_UTC), y = y)) +
    ggplot2::xlab('Day') +
    ggplot2::coord_cartesian(ylim = ylim,
                             xlim = c(1, 366)) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
}

plt.hist <- function(x, xlim = quantile(x, probs = c(0.001, 0.999))) {
  force(x)
  binwidth <- 1.5 * IQR(x) / length(x) ^ (1/3)
  ggplot2::ggplot(mapping = ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = 'black') +
    ggplot2::xlab('PRESSURE_DIFF') +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::theme_light() +
    ggplot2::theme(#axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())
}

plt.comp <- function(TIMESTAMP_UTC, Y, Y_FIT_OPT, Y_FIT_1SE, Y_FIT_1SE_LABEL,
                     front_layer = NULL, ylim, xlim, Y.col = 'red', Y.lab = 'PRESSURE_DIFF') {
  force(TIMESTAMP_UTC); force(Y)
  force(Y_FIT_OPT); force(Y_FIT_1SE); force(Y_FIT_1SE_LABEL)
  force(ylim); force(xlim)
  ggplot2::ggplot(mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = Y)) +
    front_layer +
    ggplot2::geom_line(col = Y.col, alpha = 0.9) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = Y_FIT_OPT), col = 'blue', size = 1.3) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = Y_FIT_1SE), col = 'green', size = 1.3) +
    ggplot2::coord_cartesian(ylim = ylim,
                             xlim = xlim) +
    ggplot2::annotate(
      "label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4, col = 'green', hjust = 0, vjust = 1, fill = 'grey', label.size = NA,
      label = Y_FIT_1SE_LABEL
    ) +
    ggplot2::ylab(Y.lab) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
}

periods.label <- function(coefs) {
  p <- coef.periods(coefs)
  has.trend <- any(grepl(pattern = 'trend', x = rownames(coefs), ignore.case = TRUE))
  str <- if (length(p) == 0L) 'Periods: /' else sprintf('Periods: %s days', paste0(p, collapse = ', '))
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

  p.single.fspec <- plt.fspec(periods.sec = periods, intensity = Mod(fd.single)/nrow(df.logger))

  fit.single <- fit.glmnet(y = df.logger$PRESSURE_VALUE, x = fbase, parallel = parallel)
  #plot(fit.single)
  #coef.periods(coefs(fit.single, lambda = fit.single$lambda.min))

  df.logger[, PRESSURE_VALUE_GLMNET_MIN := glmnet::predict.glmnet(fit.single$glmnet.fit, newx = fbase, s = fit.single$lambda.min)]
  df.logger[, PRESSURE_VALUE_GLMNET_1SE := glmnet::predict.glmnet(fit.single$glmnet.fit, newx = fbase, s = fit.single$lambda.1se)]



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
    keyby = TIMESTAMP_UTC]

  # fbase is made on logger timestamps. we need to filter that one to get only the multi timestamps
  ridx.multi <- df.logger[, .(TIMESTAMP_UTC, I = 1:.N)][J(df.multi$TIMESTAMP_UTC), I]
  fbase.multi <- fbase[ridx.multi,,drop=FALSE]

  fit.multi <- fit.glmnet(y = df.multi$Q.5, x = fbase.multi, parallel = parallel)
  #plot(fit.multi)
  #coefs(fit.multi, lambda = fit.multi$lambda.1se)
  #coef.periods(coefs(fit.multi, lambda = fit.multi$lambda.1se))

  df.multi[, Q.5_GLMNET_MIN := glmnet::predict.glmnet(fit.multi$glmnet.fit, newx = fbase.multi, s = fit.multi$lambda.min)]
  df.multi[, Q.5_GLMNET_1SE := glmnet::predict.glmnet(fit.multi$glmnet.fit, newx = fbase.multi, s = fit.multi$lambda.1se)]

  fd.multi <- sapply(1/periods, FUN = dtft,
                     x = df.multi$Q.5 - median(df.multi$Q.5), timestamps = df.multi$TIMESTAMP_UTC)
  #plot(Mod(fd.multi), type = 'l', x = periods/3600/24, xlab = 'days')

  p.multi.fspec <- plt.fspec(periods.sec = periods, intensity = Mod(fd.multi)/nrow(df.multi))



  # Reference comparison -------------------------------------------------------

  df.ref <- compare(df.logger, read.baro('KNMI_20200312_hourly'))
  data.table::setkey(df.ref, TIMESTAMP_UTC)
  #plot(df.ref$PRESSURE_DIFF, type = 'l', x = df.ref$TIMESTAMP_UTC)

  # fbase is made on logger timestamps. we need to filter that one to get only the ref timestamps
  ridx.ref <- df.logger[, .(TIMESTAMP_UTC, I = 1:.N)][J(df.ref$TIMESTAMP_UTC), I]
  fbase.ref <- fbase[ridx.ref,,drop=FALSE]

  fit.ref <- fit.glmnet(y = df.ref$PRESSURE_DIFF, x = fbase.ref, parallel = parallel)
  #plot(fit.ref)
  #coef.periods(coefs(fit.ref, lambda = fit.ref$lambda.1se))

  df.ref[, PRESSURE_DIFF_GLMNET_MIN := glmnet::predict.glmnet(fit.ref$glmnet.fit, newx = fbase.ref, s = fit.ref$lambda.min)]
  df.ref[, PRESSURE_DIFF_GLMNET_1SE := glmnet::predict.glmnet(fit.ref$glmnet.fit, newx = fbase.ref, s = fit.ref$lambda.1se)]

  fd.ref <- sapply(1/periods, FUN = dtft,
                   x = df.ref$PRESSURE_DIFF - median(df.ref$PRESSURE_DIFF), timestamps = df.ref$TIMESTAMP_UTC)
  #plot(Mod(fd.ref), type = 'l', x = periods/3600/24, xlab = 'days')

  p.ref.fspec <- plt.fspec(periods.sec = periods, intensity = Mod(fd.ref)/nrow(df.ref))



  # Comparison plots -----------------------------------------------------------

  p.single <- plt.comp(
    TIMESTAMP_UTC = df.logger$TIMESTAMP_UTC, Y = df.logger$PRESSURE_VALUE,
    Y_FIT_OPT = df.logger$PRESSURE_VALUE_GLMNET_MIN, Y_FIT_1SE = df.logger$PRESSURE_VALUE_GLMNET_1SE,
    Y_FIT_1SE_LABEL = periods.label(coefs(fit.single, lambda = fit.single$lambda.1se)),
    ylim = quantile(df.logger$PRESSURE_VALUE, probs = c(0.001, 0.999)),
    xlim = range(df.logger$TIMESTAMP_UTC), Y.col = 'black', Y.lab = 'PRESSURE_VALUE'
  )

  ylim.multi <- quantile(df.multi$Q.5, probs = c(0.005, 0.995))
  ylim.ref <- quantile(df.ref$PRESSURE_DIFF, probs = c(0.005, 0.995))
  ylim.comp.range <- max(diff(ylim.multi), diff(ylim.ref))

  p.multi <- plt.comp(
    TIMESTAMP_UTC = df.multi$TIMESTAMP_UTC, Y = df.multi$Q.5,
    Y_FIT_OPT = df.multi$Q.5_GLMNET_MIN, Y_FIT_1SE = df.multi$Q.5_GLMNET_1SE,
    Y_FIT_1SE_LABEL = periods.label(coefs(fit.multi, lambda = fit.multi$lambda.1se)),
    front_layer = ggplot2::geom_point(data = df.diff, mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_DIFF), pch = '.'),
    ylim = mean(ylim.multi) + c(-1, +1)*ylim.comp.range/2, xlim = range(df.logger$TIMESTAMP_UTC)
  )

  p.ref <- plt.comp(
    TIMESTAMP_UTC = df.ref$TIMESTAMP_UTC, Y = df.ref$PRESSURE_DIFF,
    Y_FIT_OPT = df.ref$PRESSURE_DIFF_GLMNET_MIN, Y_FIT_1SE = df.ref$PRESSURE_DIFF_GLMNET_1SE,
    Y_FIT_1SE_LABEL = periods.label(coefs(fit.ref, lambda = fit.ref$lambda.1se)),
    ylim = mean(ylim.ref) + c(-1, +1)*ylim.comp.range/2, xlim = range(df.logger$TIMESTAMP_UTC))



  # Yearly plots ---------------------------------------------------------------

  p.single.yearly <- plt.yearly(TIMESTAMP_UTC = df.logger$TIMESTAMP_UTC, y = df.logger$PRESSURE_VALUE)

  p.multi.yearly <- plt.yearly(TIMESTAMP_UTC = df.multi$TIMESTAMP_UTC, y = df.multi$Q.5,
                               ylim = mean(ylim.multi) + c(-1, +1)*ylim.comp.range/2)

  p.ref.yearly <- plt.yearly(TIMESTAMP_UTC = df.ref$TIMESTAMP_UTC, y = df.ref$PRESSURE_DIFF,
                             ylim = mean(ylim.ref) + c(-1, +1)*ylim.comp.range/2)



  # Density plots --------------------------------------------------------------

  xlim.hist.multi <- quantile(df.multi$Q.5, probs = c(0.001, 0.999))
  xlim.hist.ref <- quantile(df.ref$PRESSURE_DIFF, probs = c(0.001, 0.999))
  xlim.hist.range <- max(diff(xlim.hist.multi), diff(xlim.hist.ref))

  p.multi.hist <- plt.hist(df.multi$Q.5, mean(xlim.hist.multi) + c(-1, +1)*xlim.hist.range/2)
  p.ref.hist <- plt.hist(df.ref$PRESSURE_DIFF, mean(xlim.hist.ref) + c(-1, +1)*xlim.hist.range/2)

  p.empty <- ggplot2::ggplot() + ggplot2::theme_void()



  # File export ----------------------------------------------------------------

  filename <- sprintf('./drifts/analysis_20/%s.png', tools::file_path_sans_ext(basename(logger.name)))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  layout_matrix <- rbind(c(1, 1, 1,  4,  5,  6),
                         c(2, 2, 2,  7,  8,  9),
                         c(3, 3, 3, 10, 11, 12))

  grob.title <- grid::textGrob(sprintf('%s (#%s observations from %s to %s)',
                                       basename(logger.name), nrow(df.logger),
                                       min(df.logger$TIMESTAMP_UTC), max(df.logger$TIMESTAMP_UTC)),
                               x = 0.05, hjust = 0)

  local({
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    print(gridExtra::grid.arrange(p.single, p.multi, p.ref,
                                  p.single.fspec, p.single.yearly, p.empty,
                                  p.multi.fspec, p.multi.yearly, p.multi.hist,
                                  p.ref.fspec, p.ref.yearly, p.ref.hist,
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
