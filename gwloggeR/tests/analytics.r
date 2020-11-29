# Before running this file, make sure to build and install the package. (Ctrl + Shift + B)

# Load gwloggeR.data package ----------------------------------------------

options(logger.root.data.path = './../data/raw')
try(devtools::load_all('./../gwloggeR.data', export_all = FALSE))
try(detach('package:gwloggeR.data'))

# Note that this may take some 30 minutes to complete. So run only before releasing.
lapply(
  list.files(path = './tests/analytics/', pattern = 'detect_.*\\.r', recursive = FALSE, full.names = TRUE),
  source
)
