# LOGGER ----------------------
gwloggeR.data::enumerate(partner = 'inbo')
gwloggeR.data::enumerate(partner = 'geotech')


# INBO ---------------
df <- gwloggeR.data::read('BAOL008X_7252')$df
nrow(df)

gwloggeR::detect_outliers(x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC, plot = TRUE, verbose = TRUE)
gwloggeR::detect_outliers(x = df$PRESSURE_VALUE, apriori = gwloggeR::apriori('air pressure'),
                          timestamps = df$TIMESTAMP_UTC, plot = TRUE, verbose = TRUE)

df <- gwloggeR.data::read('DYLP006X')$df
gwloggeR::detect_outliers(x = df$PRESSURE_VALUE, apriori = gwloggeR::apriori('hydrostatic pressure'),
                          timestamps = df$TIMESTAMP_UTC, plot = TRUE)

# GEOTECH --------------
df <- gwloggeR.data::read('ff_b29d_0903')$df
