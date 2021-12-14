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

# a structural engineering beam stiffness matrix
KK <- matrix(c('12 * EI/L^3', '6 * EI/L^2', '-12 * EI/L^3', '6 * EI/L^2',
              '6 * EI/L^2', '4 * EI/L', '-6 * EI/L^2', '2 * EI/L',
              '-12 * EI/L^3', '-6 * EI/L^2', '12 * EI/L^3', '-6 * EI/L^2',
              '6 * EI/L^2', '2 * EI/L', '-6 * EI/L^2', '4 * EI/L'),
            nrow = 4)
KK <- ysym(KK)

# a structural engineering beam displacement vector
# with typical notations for conversion to latex
dd <- c('v_{1}', 'theta_{1}', 'v_{x, z}', 'theta_{x, z}') %>%
  ysym()

val <- 3:8
names(val) <- c(dd, 'EI', 'L')

sub_eq <- function(eq = 'K * d', val, vars = list(K = KK, d = dd), ...) {
  if (!is.null(vars))
    list2env(vars, environment())
  list2env(list(...), environment())
  objs <- setdiff(ls(), c('eq', 'vars', 'val'))
  f <- eval(parse(text = eq))
  f_tex <- Ryacas::tex(f)
  val_names <- names(val)
  new_names <- gsub('+[\\{_,. \\}]', 'abcdefg', val_names)
  eq_unsolved <- eq
  for (i in 1:length(objs)) {
    eq_unsolved <-
      gsub(objs[i], Ryacas::tex(get(objs[i])), eq_unsolved, fixed = TRUE)
    obj <- get(objs[i])
    for (k in 1:length(obj)) {
      for (j in 1:length(val_names)) {
        obj[k] <- gsub(val_names[j], new_names[j], obj[k], fixed = TRUE)
      }
    }
    assign(objs[i], obj)
  }
  ff <- eval(parse(text = eq))
  ff_tex <- Ryacas::tex(ff)
  eq_unsolved <- gsub('*', '\\bullet', eq_unsolved, fixed = TRUE)

  val_sym <- sapply(val_names, function(x) Ryacas::tex(Ryacas::ysym(x)))
  for (i in 1:length(val)) {
    f_tex <- gsub(val_sym[i], new_names[i], f_tex, fixed = T)
    str <- paste(new_names[i], val[i], sep = ' = ')
    f_tex <- eval(parse(text = paste0('tex_sub(f_tex, ', str, ')')))
  }
  return(f_tex)
}

