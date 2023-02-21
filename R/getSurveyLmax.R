#' Title
#'
#' @param dat
#' @param sp
#' @param col.var
#'
#' @return
#' @export
#'
#' @examples
#'
getSurveyLmax <- function(dat = dat,
                          sp = "anchoveta",
                          src = "pope",
                          col.var = 4:78
                          ){
  sp <- getSpeciesInfo(sp = sp)
  if(src == "fsh"){len <- seq(sp$Lmin.fsh,sp$Lmax.fsh, sp$bin.L)}
  if(src == "pope"){len <- seq(sp$Lmin.pope, sp$Lmax.pope, sp$bin.L)}
  marks <- len
  tmp <- dat[, col.var]
  mtx <- as.matrix.data.frame(dat[, col.var])

  if(length(marks) != ncol(mtx)){
    errorCondition("length of the markings does not match the size columns")
  }

  mtx[which(mtx > 0, arr.ind = T)] <- 1
  mtx <- sweep(mtx, 2, FUN = "*", STATS = marks)

  idx <- which(rowSums(mtx, na.rm = TRUE) == 0)
  if(length(idx)>0){ mtx <- mtx[-idx, ]; dat <- dat[-idx, ] }
  out <- apply(mtx, 1, max, na.rm = TRUE)

  tmp_lmax <- rep(NA, nrow(tmp))
  for(i in 1:nrow(tmp)){
    tmp_lmax[i] <- tmp[i, marks %in% out[i]]
  }

  out <- cbind(dat[,-1*col.var], out,tmp_lmax)
  return(out)

}
