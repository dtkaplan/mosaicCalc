#' Functions from data
#' 
#' Functions that are derived from data
#' 
#' @param formula A `y ~ x` type formula referring to the input and output variables in the data
#' @param data a data frame
#' @param span a hyperparameter for smoothers describing the amount of smoothing. (Default: 0.5)
#' @param degree a hyperparameter for smoothers, 
#' @param ... other arguments (not used?)
#' @param method character string describing the specific splining method
#' @param monotonic logical. Should the spline be monotonic?
#' @rdname smoothers
#' @export
smoother = function(formula, data, span=0.5, degree=2, ... ) {
  input.names = all.vars(formula)[-1]
  L = loess(formula, data, span=span, degree=degree, ..., control=loess.control(surface="direct"))
  makeDF = paste( "data.frame( ", paste(input.names,collapse=",",sep=""),")")
  F = function() {
    D = eval(parse(text=makeDF))
    predict(L, newdata=D)
  }
  tmp = paste("alist( ", paste(input.names, "=", collapse = ",", sep = ""), ")")
  tmp = eval(parse(text = tmp))
  formals(F) = tmp
  return(F)
}
# =============================
#' @rdname smoothers
#' @export
linearModel = function(formula, data, ...) {
  input.names = all.vars(formula)[-1]
  L = lm(formula, data, ...)
  makeDF = paste( "data.frame( ", paste(input.names,collapse=",",sep=""),")")
  F = function() {
    D = eval(parse(text=makeDF))
    predict(L, newdata=D)
  }
  tmp = paste("alist( ", paste(input.names, "=", collapse = ",", sep = ""), ")")
  tmp = eval(parse(text = tmp))
  formals(F) = tmp
  return(F)
}
# =============================
#' @rdname smoothers
#' @export
interpolating.function = function(formula, data, method="fmm",monotonic=FALSE,connect=FALSE) {
  fnames = all.vars(formula)
  if( length(fnames) > 2 )
    stop("Sorry: Doesn't yet handle multiple input variables.")
  y = get(fnames[1],pos=data)
  x = get(fnames[2],pos=data)
  if( connect ) SF = approxfun(x,y,rule=2)
  else {
    if( ! monotonic )  SF = splinefun(x,y,method=method)
    else SF = splinefun(x,y,method="monoH.FC")
  }
  F = function(foobar, deriv=0 ){
    x = get(fnames[2])
    if(connect) SF(x)
    else SF(x,deriv=deriv)
  }
  if (connect) tmp = paste("alist( ", fnames[2], "=)", sep="")
  else tmp = paste("alist( ", fnames[2], "=, deriv=0)", sep="")
  formals(F) = eval(parse(text=tmp))
  return(F)
}
# ==============
#' @rdname smoothers
#' @export
spliner = function(formula, data,method="fmm",monotonic=FALSE) {
  interpolating.function(formula, data, method=method, monotonic=monotonic)
}
# ==============
#' @rdname smoothers
#' @export
connector = function(formula, data, method="linear") {
  interpolating.function(formula, data, connect=TRUE)
}