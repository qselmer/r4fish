# -------------------------------------------------------------------------
## convierte a character
asc <- function(x){
  x <- as.character(x)
  return(x)
}
# -------------------------------------------------------------------------
## convierte a numeric
asn <- function(x){
  x <- as.numeric(x)
  return(x)
}
# -------------------------------------------------------------------------
## convierte de factor a numeric
c2n <-function(x){
  x <- asn(asc(x))
  return(x)
}
# -------------------------------------------------------------------------
## convierte una matriz en un data.frame. opcion: nombre de las filas
mt2df <-  function(mt, row.nm = NA){
  df <- as.data.frame.matrix(mt)
  if(row.nm == 0){rownames(df) <-  rownames(mt)}
  if(row.nm > 0){rownames(df) <- mt[,row.nm]}
  if(is.na(row.nm)){rownames(df) <- NULL}
  return(df)
}
# -------------------------------------------------------------------------
## convierte un data.frame a una matriz. opcion: nombre de las filas
df2mt <- function(df, row.nm = NA){
  mt <- as.matrix.data.frame(df)
  if(row.nm == 0){rownames(mt) <-  rownames(df)}
  if(row.nm > 0){rownames(mt) <- df[,row.nm]}
  if(is.na(row.nm)){rownames(mt) <- NULL}
  return(mt)
}
# -------------------------------------------------------------------------
## convierte una tabla de frecuencia a un data.frame.
tb2df <- function(vec, name){
  y <- as.data.frame(table(vec))
  names(y)[1] <- name
  names(y) <- tolower(names(y))
  return(y)
}
# -------------------------------------------------------------------------
## devuelve el ultimo valor
last <- function(x){
  y <- x[length(x)]
  return(y)
}
# -------------------------------------------------------------------------
## devuelve el primer valor
first <- function(x){
  y <- x[1]
  return(y)
}
# -------------------------------------------------------------------------
## longitud de elementos únicos
lenuni <- function(vec){
  out <- length(unique(vec))
  return(out)
}
# -------------------------------------------------------------------------
## devuelve el numero en texto
number2text <- function(x){
  x <- as.character(x)
  txt <- switch(x,
                "0.25" = "quarter","0.5" = "half", "1" = "one", "2" = "two",
                "3" = "three", "4" = "four", "5" = "five",
                "Number not supported")
  return(txt)
}
# -------------------------------------------------------------------------
## devuelve el texto en el numero de lineas según split
newline <- function(x, split = " "){
  x <- unlist(strsplit(x,split))
  charx <- x[1]
  for(u in 2:length(x)){
    charx <- paste(charx, "\n", x[u], sep="")
  }
  return(charx)
}
# -------------------------------------------------------------------------
## convierte el vector en el rango de 0-1 o 0-100
normalize <- function(vect, hundred = T, na.rm = T){
  out = vect/sum(vect, na.rm = na.rm)
  if(hundred){out2 <- out*100 }else{out2 <- out}
  return(out2)
}
# -------------------------------------------------------------------------
## escala los valores a un máximo
scaleMax <- function(vect, upper = 5, na.rm = T){
  out <- max(vect, na.rm = na.rm)
  out <- vect/out
  out <- out*upper
  return(out)
}
# -------------------------------------------------------------------------
## escale NASC en base k radio
scaleNasc <- function(vect, k = 500, na.rm = T){
  out <- sqrt(vect/(pi*k))
  return(out)
}
# -------------------------------------------------------------------------
## redondea un número hacia arriba al múltiplo más cercano de to
roundUp <- function(x, to = 5){
  to*(x%/%to + as.logical(x%%to))
}
# -------------------------------------------------------------------------
## obtiene en minúscula los nombres de un data.frame
nameTw <- function(data){
  out <- tolower(names(data))
  return(out)
}
# -------------------------------------------------------------------------
## retira espacio en blanco a la izquierda y derecha para ch. data.frame
trimwsDF <- function(data){
  class.data <- unname(unlist(lapply(data, class)))
  n.class <- which(class.data == "character")
  data[, n.class] <- apply(data[, n.class],2,trimws)
  return(data)
}
# -------------------------------------------------------------------------
## agrega secuencias a un vector dado en ambos lados o solo uno
addSeq <- function(vec, add = c(2, 0)){
  rz <- diff(vec)
  if(lenuni(rz) > 1){
    errorCondition("Value sequence error")}else{
      rz <- rz[1]
    }
  if(length(add) == 1){add = rep(add, 2)}
  i1 <- rev(seq(range(vec)[1]-rz, by = -1*rz, length.out = add[1]))
  i2 <- seq(range(vec)[2]+rz, by =  1*rz, length.out = add[2])
  vec2 <- c(i1, vec,i2)
  return(vec2)
}
# -------------------------------------------------------------------------
## busca un patrón específico dentro de un vector
vectorInVector = function(pattern, tag)
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
# -------------------------------------------------------------------------
##elimina los ceros en un vector y los reemplaza con valores NA o 0
cleanZeros = function(vect, nzeros = 1){
  data = matrix(vect)
  data[data == 0] = NA

  if((sum(is.na(data)) == length(data)) | (sum(!is.na(data)) == length(data)))
    return(data)

  if(nzeros > 0)
  {
    iniIndex = apply(matrix(apply(data, 1, is.na)), 2, vectorInVector, pattern = c(TRUE, FALSE))
    finIndex = apply(matrix(apply(data, 1, is.na)), 2, vectorInVector, pattern = c(FALSE, TRUE))

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
## convierte fechas en valores numéricos que representan el año
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
## convierte valores numéricos en fechas que representan el año
year2date <- function (yeardec){
  start <- as.POSIXct(paste0(trunc(yeardec), "/01/01"), tz = "UTC")
  end <- as.POSIXct(paste0(trunc(yeardec) + 1, "/01/01"),
                    tz = "UTC")
  res <- as.Date(start + (difftime(end, start, units = "secs") *
                            (yeardec - trunc(yeardec))))
  return(res)
}
# -------------------------------------------------------------------------
## calcula la inversa de una función f
inverse <- function (f, lower = -100, upper = 100) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
  }
# -------------------------------------------------------------------------
## plotea un gráfico vacio
plotNULL <- function(mtxt = "", txt = "", col.txt = 2,  cex.txt = 1,
                     madj = 0.01, mline = 0,  box = T, line = T){
  plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
  mtext(3, txt = mtxt, line = mline, adj = madj, cex = cex.txt, font = 2)
  text(1,1,txt, cex = cex.txt*1.5, col = col.txt)
  if(line)lines(-5:5, -5:5)
  if(box) box()
  return(invisible())
}
# -------------------------------------------------------------------------
## obtiene la versión de mi paquete
getR4fish <- function (pkg = "r4fish"){
  pd <- utils::packageDescription(pkg)
  v <- paste0(pd$Package, "_v", pd$Version)
  if (is.null(pd$GithubRef)) {
    return(v)
  }
  else {
    paste0(v, "@", substr(pd$GithubSHA1, 1, 6))
  }
}
# -------------------------------------------------------------------------
## crea un pie de pagina para los plots
txtFoot <- function (string = getR4fish(), cex = 0.5, do.flag = NULL)
{
  if (is.null(do.flag)) {
    if (mean(par()$mfrow) > 1) {
      do.flag <- FALSE
    }
    else {
      do.flag <- TRUE
    }
  }
  if (!is.null(string)) {
    if (string != "" & !is.na(string) & do.flag) {
      opar <- par(new = "TRUE", plt = c(0, 1, 0, 1), mfrow = c(1,
                                                               1), xpd = FALSE)
      on.exit(par(opar))
      plot(1, typ = "n", xaxt = "n", yaxt = "n", xlab = "",
           ylab = "", bty = "n")
      on.exit(par(opar))
      plt <- par("plt")
      usr <- par("usr")
      xcoord <- usr[2] + (usr[2] - usr[1])/(plt[2] - plt[1]) *
        (1 - plt[2]) - 0.4 * strwidth("m")
      ycoord <- usr[3] - diff(usr[3:4])/diff(plt[3:4]) *
        (plt[3]) + 0.4 * strheight("m")
      if (par("xlog")) {
        xcoord <- 10^(xcoord)
      }
      if (par("ylog")) {
        ycoord <- 10^(ycoord)
      }
      text(xcoord, ycoord, string, adj = 1, cex = cex)
    }
  }
}

