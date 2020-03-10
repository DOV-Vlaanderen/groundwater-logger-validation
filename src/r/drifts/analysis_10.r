# Analysis of differences between one barometer and many others in its neighbourhood.
# This one uses the data from 04 to make a first estimate of the barometer error.

# This produces the results variable.
# To speed this up, set compute(make.plot = FALSE) in the sourced file.
source('./drifts/analysis_04.r')

list.quant <- lapply(results, function(df) {
  if (is.null(df)) return(data.table::data.table())
  df.Q <- df[, .(Q.5 = quantile(PRESSURE_DIFF, 0.5), N.GROUP = .N), by = TIMESTAMP_UTC]
  df.Q <- df.Q[N.GROUP >= 10, ]
  wiskers <- boxplot.stats(df.Q[, Q.5])$stats[c(1, 5)]
  df.Q[, Q.5.scaled := Q.5 - mean(wiskers)]
  df.Q[, FILE := basename(attr(df, 'logger.name'))]
  df.Q[, N := .N]
  df.Q[, IQR := IQR(Q.5)]
  df.Q[, WISK := wiskers[2L] - wiskers[1L]]
  df.Q[, PERC.95 := quantile(Q.5, 0.975) - quantile(Q.5, 0.025)]
  df.Q
})

local(with(list.quant[['barodata/BAOL016X_77551.csv']], {
  print(ggplot2::ggplot(mapping = ggplot2::aes(x = TIMESTAMP_UTC)) +
          ggplot2::geom_line(mapping = ggplot2::aes(y = Q.5), col = 'red', alpha = 0.5) +
          ggplot2::theme_light() +
          ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5)))
  plot(x = TIMESTAMP_UTC, y = Q.5, pch = '.')

  par(mfrow = c(1, 2))
  on.exit(par(mfrow = c(1, 1)))
  hist(Q.5, breaks = 50, main = '')
  boxplot(Q.5)
  boxplot.stats(Q.5)$stats
}))

df.quant <- data.table::rbindlist(list.quant) # 424332 observations, 414692 with df.quant[N.GROUP >= 10, ]
df.quant[, LABEL := sprintf('%s (#%i) [%.2f WISK]', FILE, N, WISK)]
df.quant <- df.quant[order(WISK, LABEL), ]
df.quant[, LABEL := factor(LABEL, levels = unique(LABEL))]

ggplot2::ggplot(data = df.quant, mapping = ggplot2::aes(y = Q.5.scaled, x = LABEL)) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip(ylim = c(-25, 25)) +
  ggplot2::ggtitle(label = 'Centered distrbiution of median curve (red) from analysis 04',
                   subtitle = NULL) +
  ggplot2::xlab(NULL) +
  ggplot2::theme_light()

ggplot2::ggsave('./drifts/analysis_10/baro_median_errors_uhd-1.png', width = 2160/96, height = 3840/96, dpi = 96)

write.csv(unique(df.quant[,.(FILE, N, IQR, WISK, PERC.95)]), file = './drifts/analysis_10/baro_median_errors.csv', row.names = FALSE)
