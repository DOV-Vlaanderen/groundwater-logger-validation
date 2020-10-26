#' @keywords internal
#'
plot_drifts.sigcolor <- function(significance) {
  if (significance < 1/1000) return('red')
  if (significance < 5/100) return('orange')
  'springgreen'
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
  p.orig <- plot_drifts.original(x, timestamps, xlim = range(timestamps))
  p.diff <- plot_drifts.differences(dr.x = dr$x, dr.ts = dr$timestamps, drift = drift, xlim = range(timestamps))
  p.empty <- ggplot2::ggplot() + ggplot2::theme_void()

  layout_matrix <- rbind(c(1),
                         c(2))

  grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)

  gridExtra::grid.arrange(p.orig, p.diff,
                          layout_matrix = layout_matrix,
                          top = grob.title)
}
