
as.c <- function(x){
  x <- as.character(x)
  return(x)
}

as.md <-  function(x){
  x <- as.matrix.data.frame(x)
  return(x)
}

# -------------------------------------------------------------------------

df2m <- function(df = NA,
                 row.n = 0) {

  if(row.n == 0){
    tmp <- df
  }else{
    rownames(df) <- df[,row.n]
    tmp <- df[, -row.n]
  }
  tmp <- amd(tmp)
  return(tmp)

}

# -------------------------------------------------------------------------
inverse <- function (f,
                     lower = -100,
                     upper = 100) {

  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]

}

