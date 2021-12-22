#' Translates `integrate`, `deriv` and `t` into `tex` equivalent
#' 
#' Function to find parenthesis pairs to catch `Ryacas` commands such as
#'   `integrate`, `t`,`deriv.yac_symbol`  which still exist in the `tex`
#'   output so that they can aslo be tralslated into `tex` for pretty 
#'   printing in `rmarkdown`
#'   
#' @param x a string
#' 
#' @export
parentheses <- function(x) {
  xx <- parentheses_fn(x, 't')
  xx <- parentheses_fn(xx, 'integrate')
  xx <- parentheses_fn(xx, 'deriv')
return(xx)
}

parentheses_fn <- function(x, term = 'integrate') {
  pos <- stringr::str_locate_all(x, paste0(term, '\\('))
  if (length(pos[[1]]) == 0)
    return(x)
  xx <- x
  res <- lapply(1:nrow(pos[[1]]), function(y) 
    bracket_count(x, pos[[1]][y, 2] + 1))
  str_chars <- strsplit(x, '')[[1]]
  new_txt <- list()
  for (i in 1:length(res)) {
    txt <- paste0(term, paste0(str_chars[res[[i]][1]:res[[i]][2]], collapse = ''))
    ex <- eval(parse(text = paste0('rlang::expr(', txt, ')')))
    if (term == 'integrate') {
      if (length(ex) == 5)
        new_txt[[i]] <- paste0('\\int_', as.character(ex[4]), '^', 
                               as.character(ex[5]), 
                          '\\left(', as.character(ex[2]), '\\right) d', 
                          as.character(ex[3]))
      else if (length(ex) == 3)
        new_txt[[i]] <- paste0('\\int ', '\\left(', as.character(ex[2]), 
                               '\\right) d', as.character(ex[3]))
      else
        return(message('something'))
    } else if (term == 'deriv') {
        exx <- as.list(ex[3])
        if (length(exx[[1]]) == 1)
          new_txt[[i]] <- paste0(paste('\\frac{\\partial}{\\partial',  
                                     exx[[1]],'}', collapse = ' '), 
                               ex[2], collapse = '  ')
        else
          new_txt[[i]] <- paste0(paste('\\frac{\\partial}{\\partial',  
                                       exx[[1]][-1],'}', collapse = ' '), 
                                 ex[2], collapse = '  ')
    } else if (term == 't') { # to do
        new_txt[[i]] <- paste0('\\left(', as.character(ex[2]), '\\right)^T', collapse = '  ')
    } else { #something
    }
      
  }
  for (i in length(new_txt):1) {
    stringr::str_sub(xx, pos[[1]][i, 1], res[[i]][2]) <- new_txt[[i]]
  }

  return(xx)
}

bracket_count <- function(x, index, start = '(', end = ')') {
  str_chars <- strsplit(x, '')[[1]]
  count <- 1
  len <- length(str_chars)
  i <- index
  while (i <= len) {
    if (str_chars[i] == start)
      count <- count + 1
    else if (str_chars[i] == end)
      count <- count - 1
    if (count == 0)
      return(c(start = index - 1, end = i))
    i <- i + 1
  }
  return(message('index not found'))
}

#' Simplify trigonometric `ysym` objects
#' 
#' Simplify `ysym` objects with trig functions to either decimal or fractions
#' 
#' @param x the `ysym` object
#' @param type if `fraction` then will return the fractional result. If 'round', will 
#'   return the decimal result
#'   
#' @return a ysym object
#' @export
simplify_trig <- function(x, type = 'fraction', ...) {
  # z <- as_r(x$yacas_cmd)
  # if (type == 'fraction') 
  #   fn <- string_fraction
  # if else (type == 'round')
  #   fn <- string_round
  # else
  #   return(x)
  # for (i in 1:nrow(x)) {
  #   for (j in 1:nrow(x)) {
  #     
  #   }
  # }
  # ysym(apply(as_r(x$yacas_cmd), c(1, 2), 
  #    function(y) { 
  #      if (type == 'round')
  #        string_round(as.character(eval(parse(text = y))), ...)
  #      else if(type == 'fraction')
  #        string_fraction(as.character(eval(parse(text = y))), ...)
  #      else
  #        as.character(eval(parse(text = y)))
  #      }))
}

#' Simplify `Ryacas` expression involving trigonometric operators
#' 
#' @param x a `Ryacas` symbolic expression
#' @param type choose either `fraction` for fractional form or
#'   `round` to round to `digits`
#' @param digits required when using `round`
#' 
#' @return a `Ryacas` symbolic expression
#' @export
simplify_ryacas <- function(x, type = 'fraction', digits = 3) {
  if (is.null(x$yacas_cmd))
    return(x)
  else if (x$is_mat) {
    d <- dim(x)
    for (i in 1:d[1]) {
      for (j in 1:d[2]) {
        x[i, j] <- simplify_fn(x[i, j], type, digits)$yacas_cmd
      }
    }
  } else if (x$is_vec) {
    d <- length(x)
    for (i in 1:d) {
      x[i] <- simplify_fn(x[i], type, digits)$yacas_cmd
    }
  } else {
    x <- simplify_fn(x, type, digits)
  }
  return(x)
}

simplify_fn <- function(x, type = 'fraction', digits = 3) {
  z <- as.character(eval(parse(text = as.character(x$yacas_cmd)))) 
  if (type == 'fraction') 
    y <- stringr::str_replace_all(
      z,
      strex::str_extract_numbers(
        z,
        sci = T,
        decimals = T,
        negs = T,
        leave_as_string = T
      )[[1]],
      as.character(MASS::fractions(
        strex::str_extract_numbers(
          z,
          sci = T,
          decimals = T,
          negs = T
        )[[1]]
      ))
    )
  else if (type == 'round')
    y <- stringr::str_replace_all(
      z,
      strex::str_extract_numbers(
        z,
        sci = T,
        decimals = T,
        negs = T,
        leave_as_string = T
      )[[1]],
      as.character(round(
        strex::str_extract_numbers(
          z,
          sci = T,
          decimals = T,
          negs = T
        )[[1]],
        digits = digits
      ))
    )
  else
    return(x)
  if (length(y) == 0)
    return(Ryacas::ysym(0))
  else
    return(Ryacas::ysym(y))
}

#' Impute a matrix into another
#' 
#' Imputes a matrix into another at the specified rows and columns
#' 
#' @param x the parent matrix which will be imputed. Note, this matrix must be at least the same
#'   dimensions as `y`
#' @param y the matrix that will be imputed into `x`
#' @param rows the rows in `x` where the rows of `y` will be imputed. Note that it is assumed that
#'   the rows in `y` are ordered so that they will go into `x` as specified by the rows argument
#' @param cols optional argument used when not dealing with a square `y` matrix in which the columns
#'   and rows for imputation are the same.Defaults to `NULL`.  When imputing finite element stiffness 
#'   matrices, this will usually be 'NULL'  Functions the same as `rows` but for columns.
#' @param replace `boolean` for whether `y` will replace `x` at these positions or be added to `x`. 
#'   Typically in FEA, added is usual so the default value of `FALSE` would be used in this case.
#'   
#' @return a matrix with the same dimensions as `x`
#' 
#' @export
impute_matrix <- function(x, y, rows, cols = NULL, replace = FALSE) {
  if (is.null(cols)) cols <- rows
  for (i in 1:length(rows))
    for (j in 1:length(cols))
      if (replace)
        x[rows[i], cols[j]] <- y[i, j]$yacas_cmd
      else
        x[rows[i], cols[j]] <- (x[rows[i], cols[j]] + y[i, j])$yacas_cmd
    return(x)
}
