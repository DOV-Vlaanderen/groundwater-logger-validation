#' @keywords interal
#'
p.val <- function(model) {
  UseMethod('p.val', model)
}


#' @keywords interal
#'
p.val.Arima <- function(model) {
  (1-pnorm(abs(model$coef[colnames(model$var.coef)])/sqrt(diag(model$var.coef))))*2
}
