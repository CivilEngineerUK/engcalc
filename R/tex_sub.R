#' Substitute variables in equations without simplifying
#' 
#' Allows the substitution but not solution of `Ryacas` equations
#'   also allowing special variable names as found in engineering 
#'   mathematics such as `theta_{x, z}` and `sigma_{(x, 1)}`.
#' 
#' @param eq the string representation of the equation to solve
#' @param ... input of `ysym` objects and variables which feature 
#'   in `eq` or in the variables that `eq` refers to.
#'   Similar to `vars` but does not require the user to
#'   input the variables as a `list`.
#' @param vars an optional `list` which holds named `ysym` objects
#'   and variables which feature in `eq` or in the variables that
#'   `eq` refers to.
#' 
#' @return a `list` of the following:
#'   1. The equation after multiplication and before substitution
#'   2. The equation after multiplication and after substitution
#'   3. The final solution
#' @export
sub_eq <- function(eq, ..., vars = NULL) {
  var_names <- c(names(unlist(vars)), names(list(...)))
  vn <- apply(sapply(var_names, function(x) x == YACAS_cmds), 2, any)
  if (any(vn))
    return(message(
      paste0('"', paste0(names(vn)[which(vn)], collapse = '", "'), 
             '"', ifelse(length(which(vn)) > 1, 
  'are YACAS commands', 'is a YACAS command') , ' reserved for YACAS operations. 
  For a list of all YACAS commands, type `engcalc::YACAS_cmds()`')))
  if (!is.null(vars))
    list2env(vars, environment())
  list2env(list(...), environment())
  objs <- setdiff(ls(), c('vn', 'var_names', names(formals())))
  objs <- objs[rev(order(nchar(objs)))]
  eq_parts <- eq
  for (i in 1:length(Ryacas_fns))
    eq_parts <- gsub(Ryacas_fns[i], '', eq_parts)
  eq_parts <- stringr::str_extract_all(eq_parts, "[a-zA-Z]+")[[1]]
  values <- setdiff(objs, eq_parts)
  eqq <- parentheses(eq)
  if (eqq == eq)
    f0 <- Ryacas::tex(Ryacas::ysym(eqq))
  else
    f0 <- eqq
  f1 <- sub_latex(eqq, eq_parts)
  f2 <- ifelse(length(values) == 0, f1, sub_latex(f1, values))
  f3 <- eval(parse(text = eq))
  f4 <- Ryacas::tex(f3)
  f5 <- sub_latex(f4, values)
  f6 <- sub_latex(f3$yacas_cmd, values, FALSE) # error here
  f7 <- f3
  f7$yacas_cmd <- f6
  f7 <- Ryacas::tex(Ryacas::simplify(f7))
  return(list(f0, f1, f4, f5, f7))
}

#' Substitute variables in equation but don't solve
#'
#' Given an equation in `TeX` form as produced by the `package:Ryacas` package,
#'   the function will substitute variables for numerical values, but not solve
#'   the equation.
#' @param x the input `TeX` string as produced by the `TeX` function in the
#'   `package:Ryacas` package
#' @param ... the names and values of variables for substitution i.e. `a = 4` will
#'   search the string for `a` and substitute it for `4` but not simplify the output
#'   
#' @examples
#' # Example 1
#' eq <- Ryacas::ysym(('(a * b) / c'))
#' tex_sub(Ryacas::tex(eq), a = 4, b = 3, c = 2)
#' @export
tex_sub <- function(x, ...) {
  vars <- list(...)
  var_names <- names(vars)
  new_names <- gsub('[[:punct:]]| ','', var_names)
  names(vars) <- new_names
  for (i in which(var_names != new_names)) {
    x <- gsub(var_names[i], new_names[i], x, fixed = T)
  }
  y <- gsubfn::gsubfn("(\\w+)", vars, x)
  z <- 'NULL'
  while(y != z){
    z <- y
    y <- gsub('  ', ' ', y)
  }
  z <- gsub("(\\d) (\\d)", "\\1 \\\\bullet \\2", z)
  gsub("(\\d) (\\d)", "\\1 \\\\bullet \\2", z)
}

#' Substitute irregular yacas expressions
#' 
#' Allows substitution of variables into `Ryacas` expressions without further operations
#'   or simplification. The function allows operations on objects which are composed 
#'   of irregular variable names such as `theta_{x,z}`.
#'   
#'   The function can output in latex form so that the working out of an equation can
#'   be shown as well as the final solution.
#' 
#' @param eq a `string` of the expression to solve i.e. `'s * a'` where
#'   both `s` and `a` are the names of `ysym` objects which are in the 
#'   parent environment (`parent.frame()`) or in another enviroment 
#'   as referenced using the `env` argument
#' @param objs a `vector or `scalar`
#' @param latex boolean for whether the output is rendered to latex using
#'   the `Ryacas::tex()` function. If `FALSE` then a string of the operations 
#' @param env the environment in which the`ysym` variables referenced in 
#'   `eq` exist. This defaults to the `parent.frame()` of where `sub_latex`
#'   is called.
#'   
#' @examples 
#' library(Ryacas)
#' library(magrittr)
#' 
#' # create some `Ryacas` variables
#' sigma <-
#' sapply(c('x', 'y', 'z'), function(x)
#'  paste('sigma_{', c('x', 'y', 'z'), ',', x, '}', sep = '')) %>%
#'  ysym()
#'
#' A <- c('A_1', 'A_2', 'A_3') %>%
#' ysym()
#' 
#' # create a string of the equation for substitution
#' eq <- 'sigma * A'
#' 
#' # create vector with variable names in
#' objs <- c('sigma', 'A')
#' 
#' # call the function to get the latex output of this
#' # unsolved equation
#' sub_latex(eq, objs)
#' 
#' # output a string of the output which can be converted to a
#' # `Ryacas` object
#' # sub_latex(eq, objs, latex = TRUE)
#' 
#' @return A tex string of the output
#' @export
sub_latex <- function(eq, objs, latex = TRUE, env = NULL) {
  if (length(objs) == 0)
    return(eq)
  if (is.null(env)) env <- parent.frame()
  fn_tex <- sapply(objs, function(x) sub_helper_fn(x, env, latex))
  if (latex) {
    objs <- sapply(objs, function(x) 
      gsub(' _', '_', Ryacas::tex(Ryacas::ysym(x))))
    eq <- gsub(' _', '_', eq)
  } else
    eq <- gsub(' ', '', eq)
  txt <- paste0(paste('"', objs, '"= "', fn_tex, '"', sep = ''), 
                collapse = ', ')
  txt <- paste0('tex_sub(eq, ', txt, ')')
  txt <- gsub('\\', '\\\\', txt, fixed = TRUE)
  txt <- eval(parse(text = txt))
  txt <- gsub(' _', '_', txt)
  if (latex)
    gsub('*', '', txt)
  else
    txt
}

sub_helper_fn <- function(x, env, latex) {
  obj <- get(x, envir = env)
  if (is.numeric(obj))
    return(obj)
  if (!any(class(obj) == 'yac_symbol')) 
    obj <- Ryacas::ysym(obj)
  if (latex)
    Ryacas::tex(obj)
  else
    obj
}