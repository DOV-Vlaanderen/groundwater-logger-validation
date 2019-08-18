df.diffs <- readRDS('./differencing/df.diffs.rds')

df <- data.table::rbindlist(lapply(names(df.diffs), function(tdiff.name) {
  data.table::data.table('TIMEDIFF' = as.numeric(tdiff.name),
                         'VALUE' = unlist(unname(df.diffs[[tdiff.name]])))
}), use.names = TRUE, idcol = FALSE)

df.ecdf <- df[, .(ECDF = list(ecdf(VALUE))), by = TIMEDIFF]
data.table::setkey(df.ecdf, TIMEDIFF)

object.size(df.ecdf)

df[TIMEDIFF==600,range(VALUE)]
tmp <- df.ecdf[TIMEDIFF == 600, ECDF][[1]]
plot(tmp)

ggplot2::ggplot(data = data.frame(x = 600, y = seq(-5, 5, length.out = 100), density = tmp(seq(-5, 5, length.out = 100))),
                mapping = ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_raster(mapping = ggplot2::aes(fill = density), interpolate = TRUE) +
  ggplot2::scale_fill_gradient2(low = 'red', mid = 'green', high = 'red', midpoint = 0.5)



outer(1:2, 3:4, FUN = function(x, y) browser())

# x = TIMEDIFF, y = VALUE
dens <- Vectorize(function(x, y) {
  data.frame('density' = df.ecdf[TIMEDIFF == x, ECDF][[1]](y), x, y)
}, vectorize.args = 'x', SIMPLIFY = FALSE)

dens(c('600', '1800'), 10:12)
as.vector(dens(c('600', '1800'), 10:12))

grad <- dens(x = df.ecdf[, TIMEDIFF], y = seq(-150, 150, length.out = 1000))
df.grad <- data.table::rbindlist(grad)

ggplot2::ggplot(data = df.grad, mapping = ggplot2::aes(x = x/60/60, y = y)) +
  ggplot2::geom_raster(mapping = ggplot2::aes(fill = density), interpolate = TRUE, alpha = 0.8) +
  ggplot2::scale_fill_gradient2(low = 'white', mid = 'red', high = 'white', midpoint = 0.5) +
  ggplot2::theme_minimal()

ggplot2::ggplot(data = df.grad[x <= 1*60*60,], mapping = ggplot2::aes(x = x/60/60, y = y)) +
  ggplot2::geom_raster(mapping = ggplot2::aes(fill = density), interpolate = TRUE, alpha = 0.8) +
  ggplot2::scale_fill_gradient2(low = 'white', mid = 'red', high = 'white', midpoint = 0.5) +
  ggplot2::theme_minimal() + ggplot2::ylim(c(-20,20))
