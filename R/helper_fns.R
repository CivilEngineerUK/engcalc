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
  pos <- stringr::str_locate_all(f1, term)
  if (length(pos) == 0)
    return(x)
  
  # now use the bracket counter function
  res <- lapply(pos, function(y) bracket_count(x, y$end + 2))
  #positions <- 
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
      return(i)
    i <- i + 1
  }
  return(message('index not found'))
}
