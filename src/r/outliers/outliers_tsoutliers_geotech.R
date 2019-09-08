library(gwloggeR)
library(tsoutliers)

local({
  print(Sys.time())
  for (f in gwloggeR.data::enumerate('geotech')) {
    print(basename(f))

    tso.path <- paste0('./tsoutliers/tsoutliers_geotech/', basename(f), '.tsoutliers')
    tso <- readRDS(tso.path)
    error <- inherits(tso, 'try-error')

    if (error) {
      y <- as.vector(as.list(attr(tso, 'condition')$call)$x)
      ao.ind <- NULL
    } else {
      y <- as.vector(tso$y)
      ao.ind <- tso$outliers[tso$outliers$type == 'AO', 'ind']
    }
    n <- length(y)
    o <- rep(FALSE, n)
    o[ao.ind] <- TRUE

    p0 <- ggplot2::ggplot(data = data.frame('x' = 1:n,
                                            'y' = y,
                                            'outliers' = o), mapping = ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_line() +
      ggplot2::geom_point(mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE) +
      ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
      #ggplot2::geom_hline(yintercept = attr(outliers, 'cutpoints'), color = 'red') +
      ggplot2::ylab('x') + ggplot2::xlab('timestamp') +
      ggplot2::theme_light()

    p1 <- ggplot2::ggplot(data = data.frame('x' = 1:n,
                                            'y' = c(NA, diff(y)),
                                            'outliers' = o), mapping = ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_line() +
      ggplot2::geom_point(mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE) +
      ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
      #ggplot2::geom_hline(yintercept = attr(outliers, 'cutpoints'), color = 'red') +
      ggplot2::ylab('diff') + ggplot2::xlab('timestamp') +
      ggplot2::theme_light()

    layout_matrix <- rbind(c(1),
                           c(2))
    tt <- paste0(if (error) paste(attr(tso, 'condition')$message, ', '),
                  basename(f), ' - ',
                  if (!error) forecast:::arima.string(tso$fit) else '/',
                  ', N = ', n)
    grob.title <- grid::textGrob(tt, x = 0.05, hjust = 0)

    local({
      png(paste0('./outliers/outliers_tsoutliers_geotech/', basename(f), '.png'), width = 1280, height = 720)
      on.exit(dev.off())

      gridExtra::grid.arrange(p0, p1, layout_matrix = layout_matrix,
                              top = grob.title)
    })
  }
  print(Sys.time())
})
