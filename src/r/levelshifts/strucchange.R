source("./data.R")

library(doSNOW)
cl <- parallel::makeCluster(6)
registerDoSNOW(cl)

local({
  print(Sys.time())
  pdf(file = './levelshifts/strucchange.pdf', width = 14, height = 7, compress = FALSE)
  for (f in get_loggers(partner = 'inbo')) {
    print(basename(f))
    df <- Logger(f)$df

    # remove no-timestamp
    df <- df[!is.na(DRME_OCR_UTC_DTE),]
    if(nrow(df) == 0L) next()

    b <- strucchange::breakpoints(formula = DRME_DRU ~ 1, data = df, hpc = "foreach")

    df[, FITTED := fitted(b)]
    df[, RESIDUALS := residuals(b)]

    require(ggplot2)
    gglabels <- labs(title = basename(f),
                     subtitle = NULL,
                     caption = NULL)

    p <- ggplot(data = df, mapping = aes(x = DRME_OCR_UTC_DTE, y = DRME_DRU)) +
      geom_line(color = 'grey60') +
      geom_line(mapping = aes(y = FITTED, group = as.factor(FITTED)), size = 1) +
      gglabels

    print(p)
  }
  dev.off()
  print(Sys.time())
})

parallel::stopCluster(cl)


df <- Logger('BAOL008X_72528')$df
b <- strucchange::breakpoints(formula = DRME_DRU ~ 1, data = tmp)
df[, FITTED := fitted(b)]
df[, RESIDUALS := residuals(b)]

spectrum(df$DRME_DRU)

require(ggplot2)
ggplot(data = df, mapping = aes(x = DRME_OCR_UTC_DTE, y = DRME_DRU)) +
  geom_line(color = 'grey60') +
  geom_line(mapping = aes(y = FITTED, group = as.factor(FITTED)), size = 1)



