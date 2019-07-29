get.diff <- function(f, timediff = 5) {
  df <- Logger(f)$df
  df <- df[!is.na(TIMESTAMP_UTC),]
  data.table::setkey(df, TIMESTAMP_UTC)

  tdiff <- unique(diff(df$TIMESTAMP_UTC))
  tdiff <- tdiff[!is.na(tdiff)]
  stopifnot(length(tdiff) == 1L)

  left <- abs(c(NA, diff(df$PRESSURE_VALUE)))
  right <- c(left[-1], NA)
  mindis <- pmin(left, right, na.rm = TRUE)

  if (tdiff == timediff) return(structure(mindis, 'ts' = df$TIMESTAMP_UTC, 'x' = df$PRESSURE_VALUE))
  NULL
}
df5 <- sapply(Logger::enumerate('geotech'), FUN = get.diff, simplify = FALSE, USE.NAMES = TRUE, timediff = 5)
df15 <- sapply(Logger::enumerate('geotech'), FUN = get.diff, simplify = FALSE, USE.NAMES = TRUE, timediff = 15)
diffvec5 <- unlist(df5)
diffvec15 <- unlist(df15)
#tmp <- df[[1]]
#table(tmp)




hist(diffvec5[diffvec5 < 15], probability = TRUE)
hist(diffvec15[diffvec15 < 30], probability = TRUE)
fdens5 <- density(diffvec5[diffvec5 < 15])
fdens15 <- density(diffvec15[diffvec15 < 30])
plot(fdens5, col = 'red')
plot(fdens15, col = 'red')

lim5 <- 10
lim15 <- 30

# lapply(1:length(df5), function(i) {
#   name <- basename(names(df5)[i])
#   x <- df5[[i]]
#   if (is.null(x)) return()
#   plot(density(x), xlim = c(0, 4), main = paste0(name, ' - ', length(x)))
#   lines(fdens5, col = 'red')
# })
#
# plot(df5[[length(df5) - 2]], ylim = c(0,1))
# table(df5[[length(df5) - 2]])
#
# dfl <- Logger('837_T4175')$df
# plot(dfl$TIMESTAMP_UTC, dfl$PRESSURE_VALUE)

local({
  dfa <- df15 # change to 5 or 15
  lim <- lim15 # change to 5 or 15
  print(Sys.time())
  for (f in names(dfa)) {
    print(f)
    vec <- dfa[[f]]
    if (is.null(vec)) next()

    n <- length(vec)
    o <- vec > lim
    p0 <- ggplot2::ggplot(data = data.frame(x = attr(vec, 'ts'),
                                      y = attr(vec, 'x'), 'outliers' = o), mapping = ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_line() +
      ggplot2::geom_point(mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE) +
      ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
      #ggplot2::geom_hline(yintercept = attr(outliers, 'cutpoints'), color = 'red') +
      ggplot2::ylab('x') + ggplot2::xlab('timestamp') +
      ggplot2::theme_light()

    p1 <- ggplot2::ggplot(data = data.frame(x = attr(vec, 'ts'),
                                      y = vec, 'outliers' = o), mapping = ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_line() +
      ggplot2::geom_point(mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE) +
      ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
      ggplot2::geom_hline(yintercept = lim, color = 'red') +
      ggplot2::ylab('min(diff-left, diff-right)') + ggplot2::xlab('timestamp') +
      ggplot2::theme_light()

    layout_matrix <- rbind(c(1),
                           c(2))
    grob.title <- grid::textGrob(paste0(basename(f), ' - v0.04'), x = 0.05, hjust = 0)

    local({
      png(paste0('./outliers/outliers_v0.04_geotech/', basename(f), '.png'), width = 1280, height = 720)
      on.exit(dev.off())

      gridExtra::grid.arrange(p0, p1, layout_matrix = layout_matrix,
                              top = grob.title)
    })
  }
  print(Sys.time())
})


