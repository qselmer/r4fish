#
as.c <- function(x){
  x <- as.character(x)
  return(x)
}

#
as.nc <-function(x){
  x <- as.numeric(as.character(x))
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
as.m2df <-  function(x, header = T){
  namesx <- colnames(x)
  x <- as.data.frame.matrix(x)
  names(x) <- namesx
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
  tmp <- as.matrix.data.frame(tmp)
  return(tmp)
}

#
len.uni <- function(vec){
  xx <- length(unique(vec))
  return(xx)
}


#
as.df.table <- function(x, name){
  y <- as.data.frame(table(x))
  names(y)[1] <- name
  names(y) <- tolower(names(y))
  return(y)
}

#
number2text <- function(x){
  x <- as.character(x)
  txt <- switch(x,
                "0.25" = "quarter","0.5" = "half", "1" = "one", "2" = "two",
                "3" = "three", "4" = "four", "5" = "five",
                "Number not supported")
  return(txt)
}

# -------------------------------------------------------------------------
plot.invi <- function(text = "", mtext = "", col = 2,  cex = 1, box = T, line = T){
  plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
  mtext(3, text = mtext, line = 0, adj = 0.01, cex = cex, font = 2)
  text(1,1,text, cex = cex*1.5, col = col)
  if(line)lines(-5:5, -5:5)
  if(box) box()
  return(invisible())
}

# -------------------------------------------------------------------------

#
newline <- function(x, split = " "){
  # x <-  "zzzz aaaa ooo"
  x <- unlist(strsplit(x,split))
  charx <- x[1]
  for(u in 2:length(x)){
    charx <- paste(charx, "\n", x[u], sep="")
  }
  return(charx)
}

# -------------------------------------------------------------------------

#
inverse <- function (f, lower = -100, upper = 100) {

  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]

}


# -------------------------------------------------------------------------

#
normalize = function(vect, na.rm = T){
  out = vect/sum(vect, na.rm = na.rm)
  return(out)
}


# -------------------------------------------------------------------------

#
add.seq <- function(vec, add = c(2, 0)){

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

#
roundUp = function(x, to = 2){
  to*(x%/%to + as.logical(x%%to))
}


# -------------------------------------------------------------------------


VectorInVector = function(pattern, tag)
  {
  lenTag = length(pattern) - 1
  result = NULL
  for(i in seq(length(tag) - lenTag))
  {
    if(isTRUE(identical(tag[seq(i, i + lenTag)], pattern)))
      result = c(result, i)
  }
  return(result)
}

#
cleanZeros = function(vect, nzeros = 1){
  data = matrix(vect)
  data[data == 0] = NA

  if((sum(is.na(data)) == length(data)) | (sum(!is.na(data)) == length(data)))
    return(data)

  if(nzeros > 0)
  {
    iniIndex = apply(matrix(apply(data, 1, is.na)), 2, VectorInVector, pattern = c(TRUE, FALSE))
    finIndex = apply(matrix(apply(data, 1, is.na)), 2, VectorInVector, pattern = c(FALSE, TRUE))

    index.fill = which(!is.na(data))
    index = NULL
    for(k in seq_len(length(iniIndex)))
      index = c(index, seq(iniIndex[k] - nzeros + 1, iniIndex[k]))
    for(k in seq_len(length(finIndex)))
      index = c(index, seq(finIndex[k] + 1, finIndex[k] + nzeros))
    index = index[!index %in% index.fill]

    data[index] = 0
  }
  return(data)
}


# -------------------------------------------------------------------------

date2year <- function (date){
  if (any(!inherits(date, c("POSIXt", "POSIXct", "POSIXlt",
                            "Date")))) {
    stop("date(s) not in POSIXt or Date format")
  }
  Y <- as.numeric(format(date, format = "%Y"))
  start <- as.POSIXct(paste0(Y, "/01/01"), tz = "UTC")
  end <- as.POSIXct(paste0(Y + 1, "/01/01"), tz = "UTC")
  sofar <- as.numeric(difftime(date, start, units = "secs"))
  total <- as.numeric(difftime(end, start, units = "secs"))
  res <- Y + sofar/total
  return(res)
}

# -------------------------------------------------------------------------



