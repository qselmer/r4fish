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
addSeq <- function(sec, add = 2){
  rz <- diff(sec)[1]
  i1 <- rev(seq(range(sec)[1]-rz, by = -1*rz, length.out = add))
  i2 <- seq(range(sec)[2]+rz, by =  1*rz, length.out = add)
  sec2 <- c(i1, sec,i2)
  return(sec2)
}
