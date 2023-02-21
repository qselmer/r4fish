getJuvSpecie <- function(dat = dat_marks,
                         a_b = dat_lp,
                         sp = "anchoveta",
                         src = "pope",
                         col.var = 4:ncol(dat_marks),
                         col.ab = 4:ncol(dat_lp),
                         type = "N",
                         unitA = 1e6,
                         abs = FALSE
                         ){
  sp <- getSpeciesInfo(sp = sp)
  if(src == "fsh"){marks <- seq(sp$Lmin.fsh,sp$Lmax.fsh, sp$bin.L)}
  if(src == "pope"){marks <- seq(sp$Lmin.pope, sp$Lmax.pope, sp$bin.L)}
  mjuv <- sp$juvenile

  tmp <- dat[, col.var]
  mtx <- as.matrix.data.frame(dat[, col.var])

  if(length(marks) != ncol(mtx)){
    errorCondition("length of the markings does not match the size columns")
  }

  if(is.data.frame(a_b)){
    a_b = as.matrix.data.frame(a_b[, col.ab])
  }else{
    a_b = cbind(alp = rep(sp$a, nrow(mtx)), blp = rep(sp$b, nrow(mtx)))
  }

  dfb <- matrix(data = NA, nrow = nrow(mtx), ncol = length(marks))

  if(type == "B"){
    for(u in 1: nrow(mtx)){
      dfb[u,] <- (a_b[u,1]*(marks^a_b[u,2]))*(mtx[u,])*(unitA/1e6)
    }
    mtx <- dfb
  }

  juv <- apply(mtx, 1, calculateJuv, abs = abs, marks = marks, mjuv = mjuv)
  out <- cbind(dat[,-1*col.var], juv)
  return(out)
}
