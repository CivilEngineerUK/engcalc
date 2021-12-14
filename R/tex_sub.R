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


#' Substitute variables in equations
#' 
#' Allows the substitution but not solution of `Ryacas` equations
#'   also allowing special variable names as found in engineering 
#'   mathematics such as `theta_{x, z}`
#' 
#' @param eq the string representation of the equation to solve
#' @param val a vector where each value has a corresponding name 
#'   of a variable which appears in the equation to be solved
#' @param vars a list which the user can use to input the parts 
#'   of the equation
#' @param ... similar to `vars` but does not require the user to
#'   input the variables in list form
#' @examples 
#' library(Ryacas)
#' library(magrittr)
#' 
#' xyz <- c('x', 'y', 'z')
#'
#' # stress tensor
#' sigma <-
#'   sapply(xyz, function(x)
#'     paste('theta_{', xyz, ',', x, '}', sep = ''))
#' areas <- c('A_1', 'A_2', 'A_3')
#' 
#' vals <- 1:12
#' names(vals) <- c(sigma, areas)
#' 
#' # calculate the effective force 
#' sub_eq('s * a', vals, s = ysym(sigma), a = ysym(areas))
#' 
#' @return a `list` including the following:
#'   1. The equation after multiplication and before substitution
#'   2. The equation after multiplication and after substitution
#'   3. The final solution
#' @export
sub_eq <-
  function(eq,
           val,
           vars = NULL,
           ...) {
    if (!is.null(vars))
      list2env(vars, environment())
    list2env(list(...), environment())
    objs <- setdiff(ls(), names(formals()))
    f <- eval(parse(text = eq))
    f_tex <- Ryacas::tex(f)
    val_names <- names(val)
    val_names <- gsub(' ', '', val_names)
    new_names <- gsub('+[\\{_,. \\}]', 'abcdefg', val_names)
    for (i in 1:length(objs)) {
      obj <- get(objs[i])
      if (!is.null(obj$yacas_cmd))
        obj <- obj$yacas_cmd
      obj <- gsub(' _', '_', obj, fixed = TRUE)
      for (j in 1:length(val_names)) {
        obj <- gsub(val_names[j], new_names[j], obj, fixed = T)
      }
      assign(objs[i], Ryacas::ysym(obj))
    }
    ff <- eval(parse(text = eq))
    ff_tex <- Ryacas::tex(ff)
    for (i in 1:length(val_names)) {
      ff_tex <-
        gsub(paste0('\\mathrm{ ', new_names[i], ' }'),
             new_names[i],
             ff_tex,
             fixed = TRUE)
      ff_tex <-
        eval(parse(text = paste0(
          'tex_sub(ff_tex, ', new_names[i], ' = ', val[i], ')'
        )))
      ff <- Ryacas::with_value(ff, new_names[i], val[i])
    }
    return(list(f_tex, ff_tex, ff))
  }
