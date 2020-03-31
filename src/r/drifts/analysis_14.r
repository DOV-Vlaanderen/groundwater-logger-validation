# Altitude analysis in continuation with v08. Here we compare the barometers
# which are on the same location.

source('./drifts/analysis_08.r', encoding = 'UTF-8')

location <- function(logger.name) {
  sub('[^/]*/([^_]+).*', '\\1', logger.name)
}

df.altitude <- data.table::data.table('location' = location(names(altitude.m.bf)),
                                      'altitude.m.bf' = altitude.m.bf)

# DIFFERENCE BETWEEN AVERAGE PRESSURE ON THE SAME LOCATION
df.altitude[, altitude.diff.m.bf := max(altitude.m.bf) - min(altitude.m.bf), by = location]
df.altitude[, N := .N, by = location]
df.altitude[!duplicated(location), !'altitude.m.bf'][altitude.diff.m.bf != 0, ][order(altitude.diff.m.bf, decreasing = TRUE), ]

plot.location <- function(location, df.list) {
  df.sel <- df.list[location(names(df.list)) == location]

  plot.baro <- function(df) {
    med <- median(df$PRESSURE_VALUE)
    ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_VALUE)) +
      ggplot2::geom_line(color = 'grey90') +
      ggplot2::geom_hline(yintercept = med) +
      ggplot2::annotate("text", vjust = -0.25, hjust = 0,
                        x = min(df$TIMESTAMP_UTC), y = med,
                        label = sprintf('Median: %0.2f cmH2O @ %.2fm (MSL)', med,
                                        altitude.m.bf[sprintf('barodata/%s', unique(df$FILE))])) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                     plot.title = ggplot2::element_text(size = 10)) +
      ggplot2::ggtitle(sprintf('%s (%i observations)', unique(df$FILE), nrow(df)))
  }
  plots <- lapply(df.sel, plot.baro)

  df <- data.table::rbindlist(df.sel, use.names = TRUE)
  p.box <- ggplot2::ggplot(df, ggplot2::aes(y = PRESSURE_VALUE, x = FILE)) +
    ggplot2::geom_hline(yintercept = bf$P.Pa_to_cmH2O(bf$P.lr(0)), size = 1, col = 'darkblue', linetype = 'dotted') +
    ggplot2::geom_hline(yintercept = bf$P.Pa_to_cmH2O(bf$P.lr(50*c(-1,1))), size = 1, col = 'green', linetype = 'dotted') +
    ggplot2::geom_hline(yintercept = bf$P.Pa_to_cmH2O(bf$P.lr(100*c(-1,1))), size = 1, col = 'red', linetype = 'dotted') +
    ggplot2::geom_boxplot() +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
                   axis.title.x = ggplot2::element_blank())

  layout_matrix <- matrix(rep(1L:length(plots), 2L), ncol = 2)
  layout_matrix <- cbind(layout_matrix, length(plots) + 1L)


  altitude.diff.max <- max(altitude.m.bf[names(df.sel)]) - min(altitude.m.bf[names(df.sel)])

  grob.title <- grid::textGrob(sprintf('%s: max difference in altitude: %.2fm', location, altitude.diff.max),
                               x = 0.05, hjust = 0)

  do.call(gridExtra::grid.arrange, c(unname(plots),
                                     list(p.box,
                                          layout_matrix = layout_matrix,
                                          top = grob.title)))

}

local({
  locations <- df.altitude[!duplicated(location), ][altitude.diff.m.bf != 0, location]

  save.plot <- function(filename, plot) {
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    plot
  }

  for (loc in locations) {
    alt.diff.m <- df.altitude[location == loc, altitude.diff.m.bf][1L]
    filename <- sprintf('./drifts/analysis_14/LOC_%03.0fm_%s.png', alt.diff.m, loc)
    save.plot(filename, plot.location(loc, df.list = df.list))
  }
})
