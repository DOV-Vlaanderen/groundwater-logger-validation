#' @keywords internal
#'
plot_drifts.sigcolor <- function(significance) {
  if (is.null(significance) || is.na(significance)) return('springgreen')
  if (significance < 1/1000) return('red')
  if (significance < 1/100) return('orange')
  'springgreen'
}


#' @keywords internal
#'
plot_drifts.samplingrate <- function(timestamps, hline.h = NULL,
                                     xlim = range(timestamps)) {

  # y-axis log transformation functions
  log12 <- function(x) logb(x = x, base = 12)
  log12inv <-  function(x) 12^x

  ts.diff.h <- round(diff(as.numeric(timestamps))/3600)
  freq <- data.table::data.table(ts.diff.h)[ts.diff.h != 0, .(.N), by = ts.diff.h][order(N, decreasing = TRUE), ]
  zero <- ts.diff.h == 0

  # Determining breaks for y-axis on log12 scale: lbreaks
  lrange <- diff(range(log12(freq$ts.diff.h)))/4
  lbreaks <- numeric()
  for (lbr in log12(freq$ts.diff.h)) {
    if (!any(lbr > lbreaks - lrange/2 & lbr < lbreaks + lrange/2)) {
      lbreaks <- c(lbreaks, lbr)
    }
  }

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = timestamps[-1][!zero], y = ts.diff.h[!zero])) +
    ggplot2::geom_hline(yintercept = hline.h, col = 'red', size = 0.2, alpha = 0.2) +
    ggplot2::geom_point(pch = '-', size = 8) +
    ggplot2::scale_y_continuous(
      breaks = log12inv(lbreaks),
      #labels = paste0(unique(ts.diff.h), 'h'),
      minor_breaks = NULL,
      trans = scales::trans_new(
        name = 'log12',
        transform = log12,
        inverse = log12inv)) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot.theme()

  p
}


#' @keywords internal
#'
plot_drifts.dtft <- function(x, timestamps, drift, remove.drift = FALSE) {

  periods.sec <- seq(from = 2*24*3600,
                     # take minimum a year for the periods
                     to = max(diff(as.numeric(range(timestamps))), 1.1*365.25*24*3600),
                     length.out = 10000)

  x.centered <- x - median(x)
  if (remove.drift && attr(drift, 'is.drifting')) {
    x.drift <- attr(drift, 'rate')*model_drifts.trend(timestamps, start.ts = attr(drift, 'timestamp'))
    x.centered <- x.centered - x.drift
  }

  freqdomain <- sapply(1/periods.sec, FUN = dtft, x = x.centered, timestamps = timestamps)
  intensity <- Mod(freqdomain)/length(x)

  # Determine peaks in the intensity curve
  df <- data.table::data.table(periods.sec, intensity)#[order(intensity, decreasing = TRUE),]
  df[, lup := c(TRUE, intensity[-1] >= intensity[-.N])]
  df[, rdown := c(intensity[-.N] >= intensity[-1], TRUE)]
  df.peaks <- df[lup & rdown, ][order(intensity, decreasing = TRUE)]

  # Set x-axis breaks based on intensity peaks
  brange <- diff(range(periods.sec))/3
  breaks <- numeric()
  while (nrow(df.peaks) > 0L) {
    breaks <- c(breaks, df.peaks[1L, periods.sec])
    df.peaks <- df.peaks[periods.sec <= tail(breaks, 1L) - brange/2 | periods.sec >= tail(breaks, 1L) + brange/2, ]
  }

  ggplot2::ggplot(mapping = ggplot2::aes(x = periods.sec/3600/24, y = intensity)) +
    ggplot2::geom_line(col = 'black') +
    ggplot2::geom_vline(xintercept = 365.25, col = 'red') +
    #ggplot2::ylab('Intensity') +
    #ggplot2::xlab('Period (days)') +
    #ggplot2::ggtitle('Discrete-Time Fourier Transform') +
    ggplot2::scale_x_continuous(breaks = round(breaks/3600/24), minor_breaks = NULL) +
    ggplot.theme()
}


#' @keywords internal
#'
plot_drifts.yearly <- function(x, timestamps, drift, remove.drift = FALSE,
                               ylim = quantile(x, probs = c(0.005, 0.995))) {

  # force(ylim) # compute ylim on given x, not the later adjusted one!

  if (remove.drift && attr(drift, 'is.drifting')) {
    x.drift <- attr(drift, 'rate')*model_drifts.trend(timestamps, start.ts = attr(drift, 'timestamp'))
    x <- x - x.drift
  }

  mdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  mdaybreaks <- cumsum(mdays) - mdays/2
  mbreaks <- c(1, 3, 5, 8, 10, 12)

  ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = data.table::yday(timestamps), y = x), shape = 16) +
    #ggplot2::xlab('Day') +
    ggplot2::coord_cartesian(ylim = ylim,
                             xlim = c(1, 366)) +
    ggplot2::scale_x_continuous(breaks = mdaybreaks[mbreaks], labels = mbreaks,
                                minor_breaks = NULL) +
    ggplot.theme()
}


#' @keywords internal
#'
plot_drifts.original <- function(x, timestamps, xlim = range(timestamps), dr = NULL,
                                 ylim = quantile(x, c(0.005, 0.995))) {

  p <- ggplot2::ggplot()

  if (!is.null(dr))
    p <- p + ggplot2::geom_line(
      data = dr[, .(reference.x.average = median(reference.x)), by = timestamps],
      mapping = ggplot2::aes(x = timestamps, y = reference.x.average),
      col = 'red3',
    )

  p <- p + ggplot2::geom_line(mapping = ggplot2::aes(x = timestamps, y = x),
                              col = 'black', alpha = 1) +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot.theme()

  p
}


#' @keywords internal
#'
plot_drifts.differences <- function(dra, drift, dr = NULL, xlim = range(dra$timestamps),
                                    ylim = quantile(dra$x, c(0.005, 0.995))) {

  # intercept line
  mu <- rep(attr(drift, 'mu'), nrow(dra))

  # add seasonality
  sc <- attr(drift, 'year.seasonality')
  if (all(!is.na(sc)))
    mu <- mu + data.matrix(fbasis(timestamps = dra$timestamps, frequencies = 1/(365.25*3600*24))) %*% sc

  # add drift
  bp.ts <- NULL
  if (attr(drift, 'is.drifting')) {
    bp.ts <- attr(drift, 'timestamp')
    mu <- mu + attr(drift, 'rate')*model_drifts.trend(dra$timestamps, start.ts = bp.ts)
  }


  p <- ggplot2::ggplot()

  if (!is.null(dr) && nrow(dr) != nrow(dra))
    p <- p + ggplot2::geom_point(
      data = dr, mapping = ggplot2::aes(x = timestamps, y = x),
      pch = '.', col = 'grey', alpha = 0.6)

  if (attr(drift, 'is.drifting'))
    p <- p + ggplot2::annotate(
      'rect', xmin = bp.ts, xmax = max(dra$timestamps),
      ymin = -Inf, ymax = Inf, alpha = 0.1,
      fill = plot_drifts.sigcolor(attr(drift, 'significance'))
    )

  p <- p + ggplot2::geom_line(data = dra,
                              mapping = ggplot2::aes(x = timestamps, y = x),
                              col = 'steelblue', alpha = 1)

  if (!all(is.na(mu)))
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes(x = dra$timestamps, y = mu),
                                col = 'black', size = 1.5, alpha = 1)

  p <- p + ggplot2::geom_vline(xintercept = as.numeric(bp.ts),
                               col = plot_drifts.sigcolor(attr(drift, 'significance')),
                               linetype = 'dashed', size = 1.2)

  if (attr(drift, 'is.drifting'))
    p <- p + ggplot2::annotate(
      'label', x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4.5, col = 'black',
      hjust = 0, vjust = 1, fill = 'grey', alpha = 0.8, label.size = NA,
      label = sprintf('Drift p-value: %.2e, rate: %.2f %s @%s', attr(drift, 'significance'),
                      attr(drift, 'rate'), attr(attr(drift, 'rate'), 'units'),
                      attr(drift, 'timestamp'))
    )

  p <- p + ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot.theme()

  p
}


#' @keywords internal
#'
plot_drifts.refcount <- function() {
  # Here we should see how many reference barometers we have at all times,
  # and also the time differences between measurements. All in 1 plot?
  stop('Not yet implemented')
}


#' @keywords internal
#'
plot_drifts <- function(x, timestamps, dr, dra, drift, title) {

  xy.blank <- ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                             axis.title.y = ggplot2::element_blank(),
                             axis.title.x = ggplot2::element_blank())

  p.orig <- plot_drifts.original(x, timestamps, dr = dr) + xy.blank
  p.diff <- plot_drifts.differences(dra = dra, drift = drift, dr = dr, xlim = range(timestamps)) + xy.blank
  p.tsdiff <- plot_drifts.samplingrate(timestamps = timestamps, hline.h = 12) + xy.blank
  p.dtft.orig <- plot_drifts.dtft(x = x, timestamps = timestamps, drift = drift, remove.drift = TRUE) + xy.blank
  p.dtft.diff <- plot_drifts.dtft(x = dra$x, timestamps = dra$timestamps, drift = drift, remove.drift = TRUE) + xy.blank
  p.yearly.orig <- plot_drifts.yearly(x = x, timestamps = timestamps, drift = drift, remove.drift = TRUE) + xy.blank
  p.yearly.diff <- plot_drifts.yearly(x = dra$x, timestamps = dra$timestamps, drift = drift, remove.drift = TRUE) + xy.blank
  p.empty <- ggplot2::ggplot() + ggplot2::theme_void()

  layout_matrix <- rbind(c(rep(1, 4), 4, 7),
                         c(rep(2, 4), 5, 8),
                         c(rep(3, 4), 6, 9))

  grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)

  gridExtra::grid.arrange(p.tsdiff, p.orig, p.diff,
                          p.empty, p.dtft.orig, p.dtft.diff,
                          p.empty, p.yearly.orig, p.yearly.diff,
                          layout_matrix = layout_matrix,
                          top = grob.title)
}
