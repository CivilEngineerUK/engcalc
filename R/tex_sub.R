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
#' @return a `list` of the following:
#'   1. The equation after multiplication and before substitution
#'   2. The equation after multiplication and after substitution
#'   3. The final solution
#' @export
sub_eq <- function(eq, ..., vars = NULL) {
  if (!is.null(vars))
    list2env(vars, environment())
  list2env(list(...), environment())
  objs <- setdiff(ls(), names(formals()))
  objs <- objs[rev(order(nchar(objs)))]
  eq_parts <- stringr::str_extract_all(eq, "[a-zA-Z]+")[[1]]
  values <- setdiff(objs, eq_parts)
  f1 <- sub_latex(eq, eq_parts)
  f2 <- ifelse(length(values) == 0, f1, sub_latex(f1, values))
  f3 <- eval(parse(text = eq))
  f4 <- Ryacas::tex(f3)
  
  ## code below not working after changes to code above
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
  return(list(f3, f4, ff_tex, tex(ff), ff))
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
#' sub_latex(eq, objs, latex = TRUE)
#' 
#' @return A tex string of the output
#' @export
sub_latex <- function(eq, objs, latex = TRUE, env = NULL) {
  if (is.null(env)) env <- parent.frame()
  fn_tex <- sapply(objs, function(x) sub_helper_fn(x, env, latex))
  if (latex)
    objs <- sapply(objs, function(x) 
      gsub(' _', '_', Ryacas::tex(Ryacas::ysym(x))))
  txt <- paste0(paste('"', objs, '"= "', fn_tex, '"', sep = ''), collapse = ', ')
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