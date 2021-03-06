% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_f1}
\alias{plot_f1}
\title{Plots of functions}
\usage{
plot_f1(object = NULL, formula, ..., inherit = FALSE, npts = 101)
}
\arguments{
\item{object}{When chaining, this holds an object produced in the earlier portions of the chain. Most users can safely ignore this argument. See details and examples.}

\item{formula}{The formula specifying the layout of the plot, e.g. `sin(x) ~ x`. See examples 
and \link{`mosaicCore::makeFun()`}.}

\item{...}{Additional arguments. These must include specifications of the ranges or values of all parameters used in the formula.}

\item{inherit}{A logical indicating whether graphical attributes should be inherited from earlier 
plots chained into this one.}
}
\description{
Draws graphs in the ggformula system of functions of one and several variables.
}
\details{
Specify the function to be plotted as a formula. The LHS of the formula gives
the value of the function, the RHS specifies one (TO BE ADDED or two) variables to be used for 
the spatial coordinates of the plot and any faceting variables. Additional aethetics,
for instance, color, can be assigned to values in the usual ggformula way, e.g. `color = ~ z`.

All arguments to the function *must* have a value or a range of variables specified, e.g.
`x = c(0, 10)` or `a = 3` or `b = 1:5`. The input variable(TO BE ADDED: s) used for the spatial coordinates
will have a finer grid produced if just two values are given (e.g. `x = c(0, 10)`).
}
\examples{
plot_f1(sqrt(x) ~ x, x = c(0,10))
if (require(mosaicData)) {
  gf_histogram(..density.. ~ age, data = HELPrct, binwidth = 3, alpha = 0.6) \%>\%
  plot_f1(dnorm(age, mean, sd) ~ age, age = c(0,100), color = ~ sd,
    mean = mean(HELPrct$age), sd = 1:3 * sd(HELPrct$age))
  # Utility bill is quadratic in month?
  f <- makeFun(lm(totalbill ~ poly(month, 2), data = Utilities))
  gf_point(totalbill ~ month, data = Utilities, alpha = 0.6) \%>\%
   plot_f1(f(month) ~ month, color = "red")
  f2 <- makeFun(lm(totalbill ~ poly(month, 2) * ccf, data = Utilities))
  gf_point(totalbill ~ month, color = ~ ccf, data = Utilities, alpha = 0.6) \%>\%
   plot_f1(f2(month, ccf) ~ month, color = ~ ccf, ccf = 100*(0:2))
} 
}
