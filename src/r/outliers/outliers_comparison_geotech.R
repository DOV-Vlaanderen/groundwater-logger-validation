local({
  print(Sys.time())
  folder <- c("./../r/outliers/outliers_v0.01_geotech/",
              "./../r/outliers/outliers_v0.02_geotech/",
              "./../r/tsoutliers/tsoutliers_geotech/")
  pdf(file = './outliers/outliers_comparison_geotech.pdf', width = 16, height = 9, compress = TRUE)
  on.exit(dev.off())

  for (f in Logger::enumerate('geotech')) {
    print(basename(f))

    file.png <- list.files(folder, pattern = basename(f), ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
    png.list <- lapply(file.png, function(fn) grid::rasterGrob(png::readPNG(fn)))
    do.call(what = gridExtra::grid.arrange, args = c(png.list, list('ncol' = 2)))
  }
  print(Sys.time())
})
