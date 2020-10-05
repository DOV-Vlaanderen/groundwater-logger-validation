# Analysis 23 and 26 together

local({
  print(Sys.time())
  folder <- c("./drifts/analysis_23/",
              "./drifts/analysis_26/")
  logger.names <- unique(tools::file_path_sans_ext(basename(list.files(folder, pattern = '.*\\.png', ignore.case = TRUE, recursive = TRUE, full.names = TRUE))))
  pdf.filename <- './drifts/analysis_27/comparison.pdf'
  dir.create(dirname(pdf.filename), showWarnings = FALSE, recursive = TRUE)
  pdf(file = pdf.filename, width = 16, height = 9, compress = TRUE)
  on.exit(dev.off())

  for (f in logger.names) {
    print(basename(f))
    file.png <- list.files(folder, pattern = paste0(f, '.*\\.png'), ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
    png.list <- lapply(file.png, function(fn) grid::rasterGrob(png::readPNG(fn)))
    do.call(what = gridExtra::grid.arrange, args = c(png.list, list('ncol' = 2)))
  }

  print(Sys.time())
})