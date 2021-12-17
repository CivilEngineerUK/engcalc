#' YACAS command names
#' 
#' @format A vector of YACAS command names which should not be used
#'   for variable names in user experessions - only to be used in 
#'   the user expresssions as yacas commands. 
#'
#' @source \url{http://yacas.sourceforge.net/ref.book.pdf}
'YACAS_cmds'

#' `Ryacas` function names
#' 
#' @format A vector of `Ryacas` function names which are used internally
#'   so that `sub_eq` does not try to remove this part of the string when 
#'   performing the substitution.
#' 
#' @source lsf.str("package:Ryacas")
'Ryacas_fns'