# Workspace for delevopment of outliers, level shift and temporal change
# detection for hydrostatic pressure.
# ------------------------------------------------------------------------------

options(logger.root.data.path = './../data/raw')
try(devtools::load_all('./../gwloggeR.data', export_all = FALSE))
try(detach('package:gwloggeR.data'))

# df <- gwloggeR.data::read('pp09-1_120829130801_F5474')$df # LS with TC
# df <- gwloggeR.data::read('DYLP003E_80766')$df
df <- gwloggeR.data::read('151202090850_T4180')$df # slight differences between R3.2.5 and R3.6.3

data.table::setkey(df, TIMESTAMP_UTC)

gwloggeR::detect_outliers(
  x = df$PRESSURE_VALUE,
  timestamps = df$TIMESTAMP_UTC,
  apriori = gwloggeR::Apriori('hydrostatic pressure'),
  plot = TRUE
)
