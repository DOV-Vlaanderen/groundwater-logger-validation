#' @keywords internal
#'
plot_drifts.sigcolor <- function(significance) {
  if (is.null(significance) || is.na(significance)) return('springgreen')
  if (significance < 1/1000) return('red')
  if (significance < 5/100) return('orange')
  'springgreen'
}


#' @keywords internal
#'
plot_drifts.timedifferences <- function(x, timestamps, hline.h = NULL,
                                        xlim = range(timestamps)) {

  ts.diff.h <- round(diff(as.numeric(timestamps))/3600)
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = timestamps[-1], y = ts.diff.h)) +
    ggplot2::geom_hline(yintercept = hline.h, col = 'red', size = 0.2, alpha = 0.2) +
    ggplot2::geom_point(pch = '-', size = 8) +
    ggplot2::scale_y_continuous(
      breaks = unique(ts.diff.h),
      #labels = paste0(unique(ts.diff.h), 'h'),
      minor_breaks = NULL,
      trans = scales::trans_new(
        name = 'log12',
        transform = function(x) logb(x = x, base = 12),
        inverse = function(x) 12^x)) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())

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

  ggplot2::ggplot(mapping = ggplot2::aes(x = periods.sec/3600/24, y = intensity)) +
    ggplot2::geom_line(col = 'black') +
    ggplot2::geom_vline(xintercept = 365.25, col = 'red') +
    #ggplot2::ylab('Intensity') +
    #ggplot2::xlab('Period (days)') +
    #ggplot2::ggtitle('Discrete-Time Fourier Transform') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())
}


#' @keywords internal
#'
plot_drifts.yearly <- function(x, timestamps, drift, remove.drift = FALSE,
                               ylim = quantile(x, probs = c(0.005, 0.995))) {

  if (remove.drift && attr(drift, 'is.drifting')) {
    x.drift <- attr(drift, 'rate')*model_drifts.trend(timestamps, start.ts = attr(drift, 'timestamp'))
    x <- x - x.drift
  }

  ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = data.table::yday(timestamps), y = x)) +
    #ggplot2::xlab('Day') +
    ggplot2::coord_cartesian(ylim = ylim,
                             xlim = c(1, 366)) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())
}


#' @keywords internal
#'
plot_drifts.original <- function(x, timestamps, xlim = range(timestamps),
                                 ylim = quantile(x, c(0.005, 0.995))) {

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = timestamps, y = x)) +
    ggplot2::geom_line(col = 'black', alpha = 1) +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())

  p
}


#' @keywords internal
#'
plot_drifts.differences <- function(dr.x, dr.ts, drift, xlim = range(dr.ts),
                                    ylim = quantile(dr.x, c(0.005, 0.995))) {

  # intercept line
  mu <- rep(attr(drift, 'mu'), length(dr.x))

  # add seasonality
  sc <- attr(drift, 'year.seasonality')
  if (all(!is.na(sc)))
    mu <- mu + data.matrix(fbasis(timestamps = dr.ts, frequencies = 1/(365.25*3600*24))) %*% sc

  # add drift
  bp.ts <- NULL
  if (attr(drift, 'is.drifting')) {
    bp.ts <- attr(drift, 'timestamp')
    mu <- mu + attr(drift, 'rate')*model_drifts.trend(dr.ts, start.ts = bp.ts)
  }

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = dr.ts, y = dr.x))

  if (attr(drift, 'is.drifting'))
    p <- p + ggplot2::annotate(
      'rect', xmin = bp.ts, xmax = max(dr.ts),
      ymin = -Inf, ymax = Inf, alpha = 0.1,
      fill = plot_drifts.sigcolor(attr(drift, 'significance'))
    )

  p <- p + ggplot2::geom_line(col = 'steelblue', alpha = 1) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = mu), col = 'black', size = 1.5) +
    ggplot2::geom_vline(xintercept = bp.ts,
                        col = plot_drifts.sigcolor(attr(drift, 'significance')),
                        linetype = 'dashed', size = 1.2)

  if (attr(drift, 'is.drifting'))
    p <- p + ggplot2::annotate(
      'label', x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4.5, col = 'black',
      hjust = 0, vjust = 1, fill = 'grey', alpha = 0.8, label.size = NA,
      label = sprintf('Drift p-value: %.2e, rate: %.2f %s', attr(drift, 'significance'),
                      attr(drift, 'rate'), attr(attr(drift, 'rate'), 'units'))
    )

  p <- p + ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())

  p
}


#' @keywords internal
#'
plot_drifts.refcount <- function() {
  # Here we should see how many reference barometers we have at all times,
  # and also the time differences between measurements. All in 1 plot?
}


#' @keywords internal
#'
plot_drifts <- function(x, timestamps, dr, drift, title) {
  p.orig <- plot_drifts.original(x, timestamps)
  p.diff <- plot_drifts.differences(dr.x = dr$x, dr.ts = dr$timestamps, drift = drift, xlim = range(timestamps))
  p.tsdiff <- plot_drifts.timedifferences(x = x, timestamps = timestamps, hline.h = 12)
  p.dtft.orig <- plot_drifts.dtft(x = x, timestamps = timestamps, drift = drift, remove.drift = TRUE)
  p.dtft.diff <- plot_drifts.dtft(x = dr$x, timestamps = dr$timestamps, drift = drift, remove.drift = TRUE)
  p.yearly.orig <- plot_drifts.yearly(x = x, timestamps = timestamps, drift = drift, remove.drift = TRUE)
  p.yearly.diff <- plot_drifts.yearly(x = dr$x, timestamps = dr$timestamps, drift = drift, remove.drift = TRUE)
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
