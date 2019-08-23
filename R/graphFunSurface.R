#' Graph function of two variables as a surface
#' 
#' @param formula of the sort accepted by `makeFun()`
#' @param ... limits for the plot. See examples.
#' 
#' @examples 
#' graphFunSurface(
#'   sin(2*pi*t/10)*exp(-.2*x) ~ t & x, 
#'   tlim=range(0,20), xlim=range(0,10))
#' 
#' @export
graphFunSurface <- function(formula, ...) {
  mosaic::plotFun(formula, ..., surface = TRUE)
}