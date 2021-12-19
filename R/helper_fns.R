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

#' Function to find parenthesis pairs to catch `Ryacas` commands such as
#'   `integrate`, `t`,`deriv.yac_symbol`  which still exist in the `tex`
#'   output so that they can aslo be tralslated into `tex` for pretty 
#'   printing in `rmarkdown`
parentheses <- function(x, term = 'integrate') {

  # find indices of term
  pos <- stringr::str_locate_all(x, term)
  if (length(pos) == 0)
    return(x)
  
  xx <- x
  # now use the bracket counter function
  res <- lapply(1:nrow(pos[[1]]), function(y) bracket_count(x, pos[[1]][y, 2] + 2))
  
  # count back from closing bracket 
  str_chars <- strsplit(x, '')[[1]]
  new_txt <- list()
  for (i in 1:length(res)) {
    txt <- paste0(term, paste0(str_chars[res[[i]][1]:res[[i]][2]], collapse = ''))
    ex <- eval(parse(text = paste0('rlang::expr(', txt, ')', collapse = '')))
    if (length(ex) == 5)
      new_txt[[i]] <- paste0('\\int_', as.character(ex[4]), '^', as.character(ex[5]), 
                        '\\left(', as.character(ex[2]), '\\right) d', as.character(ex[3]))
    else if (length(ex) == 3)
      new_txt[[i]] <- paste0('\\int ', '\\left(', as.character(ex[2]), '\\right) d', as.character(ex[3]))
    else
      return(message('something'))
  }
  
  # replace items
  for (i in length(new_txt):1) {
    str_sub(xx, pos[[1]][i, 1], res[[i]][2]) <- new_txt[[i]]
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
