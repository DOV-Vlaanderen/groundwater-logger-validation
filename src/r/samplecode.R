# LOGGER ----------------------
Logger::enumerate(partner = 'inbo')
Logger::enumerate(partner = 'geotech')


# INBO ---------------
df <- Logger('BAOL008X_7252')$df
nrow(df)

gwloggeR::detect_outliers(x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC, plot = TRUE, verbose = TRUE)
gwloggeR::detect_outliers(x = df$PRESSURE_VALUE, apriori = gwloggeR::apriori('air pressure'),
                          timestamps = df$TIMESTAMP_UTC, plot = TRUE, verbose = TRUE)

df <- Logger('DYLP006X')$df
gwloggeR::detect_outliers(x = df$PRESSURE_VALUE, apriori = gwloggeR::apriori('hydrostatic pressure'),
                          timestamps = df$TIMESTAMP_UTC, plot = TRUE)

# GEOTECH --------------
df <- Logger('ff_b29d_0903')$df
