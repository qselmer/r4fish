#
as.c <- function(x){
  x <- as.character(x)
  return(x)
}

#
last <- function(x){
  y <- x[length(x)]
  return(y)
}

#
first <- function(x){
  y <- x[1]
  return(y)
}

#
as.m2df <-  function(x){
  x <- as.matrix.data.frame(x)
  return(x)
}

#
as.df2m <- function(df = NA, row.n = 0){
  if(row.n == 0){
    tmp <- df
    }else{
      rownames(df) <- df[,row.n]
      tmp <- df[, -row.n]
    }
  tmp <- as.data.frame.matrix(tmp)
  return(tmp)
}

#
len.uni <- function(vec){
  xx <- length(unique(vec))
  return(xx)
}

# -------------------------------------------------------------------------

#
inverse <- function (f, lower = -100, upper = 100) {

  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]

}

#
normalize = function(vect, na.rm = T){
  out = vect/sum(vect, na.rm = na.rm)
  return(out)
}

#
add_seq <- function(vec, add = c(2, 0)){

  rz <- diff(vec)
  if(len.uni(rz) > 1){
    errorCondition("Value sequence error")}else{
      rz <- rz[1]
    }

  if(length(add) == 1){add = rep(add, 2)}
  i1 <- rev(seq(range(vec)[1]-rz, by = -1*rz, length.out = add[1]))
  i2 <- seq(range(vec)[2]+rz, by =  1*rz, length.out = add[2])
  vec2 <- c(i1, vec,i2)
  return(vec2)
}
