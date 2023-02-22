#' Title
#'
#' @param sec
#' @param add
#'
#' @return
#' @export
#'
#' @examples
#'
addSeq <- function(sec, add = c(2, 0)){
  rz <- diff(sec)[1]
  if(length(add) == 1){add = rep(add, 2)}
  i1 <- rev(seq(range(sec)[1]-rz, by = -1*rz, length.out = add[1]))
  i2 <- seq(range(sec)[2]+rz, by =  1*rz, length.out = add[2])
  sec2 <- c(i1, sec,i2)
  return(sec2)
}

