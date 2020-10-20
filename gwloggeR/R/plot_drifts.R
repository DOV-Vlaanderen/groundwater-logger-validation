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
plot_drifts.differences <- function(dr.x, dr.ts, xlim = range(dr.ts),
                                    ylim = quantile(dr.x, c(0.005, 0.995))) {
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = dr.ts, y = dr.x)) +
    ggplot2::geom_line(col = 'red', alpha = 1) +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
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
  p.diff <- plot_drifts.differences(dr.x = dr$x, dr.ts = dr$timestamps, xlim = range(timestamps))
  p.empty <- ggplot2::ggplot() + ggplot2::theme_void()

  layout_matrix <- rbind(c(1),
                         c(2))

  grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)

  gridExtra::grid.arrange(p.orig, p.diff,
                          layout_matrix = layout_matrix,
                          top = grob.title)
}
