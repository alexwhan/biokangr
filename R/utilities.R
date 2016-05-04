#' Make a numeric vector from a string of comma separated numbers
#'
#' @param num_string A string vector of comma separated numbers without spaces
#'
make_num_list <- function(num_string) {
  return(lapply(strsplit(num_string, ","), as.numeric))
}
