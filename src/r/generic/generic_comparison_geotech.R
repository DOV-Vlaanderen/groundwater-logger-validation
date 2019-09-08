local({
  print(Sys.time())
  folder <- c("./../r/tsoutliers/tsoutliers_geotech/",
              "./../r/outliers/outliers_v0.04_geotech/",
              "./../r/outliers/outliers_tsoutliers_geotech/",
              "./../r/generic/generic_tsoutliers_geotech/",
              "./../r/generic/generic_v0.05_geotech/",
              "./../r/generic/generic_v0.06_geotech/")
  pdf(file = './generic/generic_comparison_geotech.pdf', width = 16, height = 9, compress = TRUE)
  on.exit(dev.off())

  for (f in gwloggeR.data::enumerate('geotech')) {
    print(basename(f))
    file.png <- list.files(folder, pattern = paste0(basename(f), '.*\\.png'), ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
    png.list <- lapply(file.png, function(fn) grid::rasterGrob(png::readPNG(fn)))
    do.call(what = gridExtra::grid.arrange, args = c(png.list, list('ncol' = 3)))
  }

  print(Sys.time())
})
