# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a
