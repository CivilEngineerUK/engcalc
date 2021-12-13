#' Substitute variables in equation but don't solve
#' 
#' Given an equation in `TeX` form as produced by the `package:Ryacas` package,
#'   the function will substitute variables for numerical values, but not solve
#'   the equation.
#' @param x the input `TeX` string as produced by the `TeX` function in the 
#'   `package:Ryacas` package
#' @param ... the names and values of variables for substitution i.e. `a = 4` will
#'   search the string for `a` and substitute it for `4` but not simplify the output
#' @examples
#' eq <- Ryacas::ysym(('(a * b) / c'))
#' eq <- Ryacas::tex(eq)
#' tex_sub(eq, a = 4, b = 3, c = 2)
#' @export
tex_sub <- function(x, ...) {
  y <- gsubfn::gsubfn("(\\w+)", list(...), x)
  gsub("(\\d) (\\d)", "\\1 \\\\times \\2", y)
}