# Function to identify NAs
#' Title
#'
#' @param pattern
#' @param tag
#'
#' @return
#' @export
#'
#' @examples
VectorInVector = function(pattern, tag){
  lenTag = length(pattern) - 1
  result = NULL
  for(i in seq(length(tag) - lenTag))
  {
    if(isTRUE(identical(tag[seq(i, i + lenTag)], pattern)))
      result = c(result, i)
  }
  return(result)
}
