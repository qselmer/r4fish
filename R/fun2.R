
ac <- function(x){
  x <- as.character(x)
  return(x)
}

amd <-  function(x){
  x <- as.matrix.data.frame(x)
  return(x)
}

# -------------------------------------------------------------------------

df2m <- function(df, row.n = 1) {

  ifelse(row.n = 0){
    tmp <- df
  }else{
    rownames(df) <- df[,row.n]
    tmp <- df[, -row.n]
  }
  tmp <- amd(tmp)
  return(tmp)

}

