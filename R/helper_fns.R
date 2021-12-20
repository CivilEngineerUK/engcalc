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
