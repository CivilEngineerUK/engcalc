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
#' # Example 1
#' eq <- Ryacas::ysym(('(a * b) / c'))
#' tex_sub(Ryacas::tex(eq), a = 4, b = 3, c = 2)
#' @export
tex_sub <- function(x, ...) {
  y <- gsubfn::gsubfn("(\\w+)", list(...), x)
  gsub("(\\d) (\\d)", "\\1 \\\\bullet \\2", y)
}

#' Same as `tex_sub` though provides a list to function
tex_sub_2 <- function(x, z) {
  y <- gsubfn::gsubfn("(\\w+)", z, x)
  gsub("(\\d) (\\d)", "\\1 \\\\bullet \\2", y)
}

# a structural engineering beam stiffness matrix
KK <- matrix(c('12 * EI/L^3', '6 * EI/L^2', '-12 * EI/L^3', '6 * EI/L^2',
              '6 * EI/L^2', '4 * EI/L', '-6 * EI/L^2', '2 * EI/L',
              '-12 * EI/L^3', '-6 * EI/L^2', '12 * EI/L^3', '-6 * EI/L^2',
              '6 * EI/L^2', '2 * EI/L', '-6 * EI/L^2', '4 * EI/L'),
            nrow = 4)
KK <- ysym(KK)

# a structural engineering beam displacement vector
# with typical notations for conversion to latex
dd <- c('v_{1}', 'theta_{1}', 'v_{x, z}', 'theta_{x, z}')

val <- 3:8
names(val) <- c(dd, 'EI', 'L')

sub_eq <- function(eq = 'K * d', val, vars = list(K = KK, d = dd), ...) {
  if (!is.null(vars))
    list2env(vars, environment())
  list2env(list(...), environment())
  objs <- setdiff(ls(), c('eq', 'vars', 'val'))
  f <- eval(parse(text = eq))
  f_tex <- Ryacas::tex(f)
  for (i in 1:length(objs)) {
    s <- gsub('+[\\{_,.\\}]',
              'abcxyz', get(objs[i]))
    assign(objs[i], gsub(' _', '', s))
  }
  ff <- eval(parse(text = eq))
  ff_tex <- Ryacas::tex(f)
  for (i in 1:length(val)) {
    ff_tex <- gsub(paste('\\mathrm{', objs[i], '}'), objs[i], ff_tex, fixed = T)
    str <- paste(names(val[i]), val[i], sep = ' = ')
    eq <- eval(parse(text = paste0('tex_sub(eq, ', str, ')')))
  }
  return(eq)
}