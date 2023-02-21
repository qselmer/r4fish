#' Title
#'
#' @param vec
#' @param marks
#' @param mjuv
#' @param abs
#'
#' @return
#' @export
#'
#' @examples
calculateJuv<- function(vec, marks,
                         mjuv, abs = T
                         ){
  idJuv <- which(marks < mjuv)
  if(abs == TRUE){
    juv <- sum(vec[idJuv], na.rm = T)
  }else{
    juv <- (sum(vec[idJuv], na.rm = T)/sum(vec, na.rm = T) )*100
  }
  return(juv)
}
