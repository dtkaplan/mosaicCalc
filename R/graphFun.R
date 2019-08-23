#' Graph a function of one or two variables
#' 
#' @export
graphFun <- function(formula, ..., labels = "top.pieces", 
                     contour = TRUE, tile = TRUE, alpha.tile = 0.4) {
  extras <- list(...)
  if (!require("ggformula")) stop("Must install ggformula package")
  # Get the arguments in order
  arguments <- as.character(all.vars(rlang::f_rhs(formula)))
  if (length(arguments) == 0)
    stop("Must provide a formula with variables on the right-hand side")
  if (length(arguments) > 2)
    stop("The formula can have only one or two arguments")
  # Look for a xlim or x (or tlim or t) in the ...
  xlim <- find_limit_argument(arguments[1], extras, default = "x")
  if (!is.null(xlim$name)) extras[xlim$name] <- NULL
    # Draw a line graph
  if (length(arguments) == 1) {
    arglist <- c(list(formula, xlim = xlim$range), extras)
    P <- do.call(ggformula::gf_fun, arglist)
  } else if (length(arguments) == 2) {
    ylim <- find_limit_argument(arguments[2], extras, default = "y")
    if (!is.null(ylim$name)) extras[ylim$name] <- NULL
    arglist <- c(
      list(formula, xlim=xlim$range, ylim = ylim$range, 
           labels = labels, alpha.tile = alpha.tile,
           tile = tile), 
      extras)
    P <- do.call(ggformula::gf_fun2d, arglist) %>%
      gf_labs(x = arguments[1], y = arguments[2]) %>%
      gf_refine(scale_fill_viridis_c(option="D"))

  }
  
  P
}

# Figure out if there is an argument in <extras> that corresponds
# to a valid xlim or ylim format
find_limit_argument <- function(varname, extras, default = "x") {
  name_possibilities <- c(varname,
                          paste0(varname, "lim"),
                          paste0(default, "lim"))
  matches <- which( name_possibilities %in% names(extras))
  if (length(matches) == 0) {
    argname <- NULL
    lim <- c(0, 1)
  } else {
    argname <- name_possibilities[matches[1]]
    lim <- extras[[argname]]
    if (length(lim) != 2) 
      stop("Graphing limits must be specified as two numbers.")
  }
  
  list(range = lim, name = argname)
}
