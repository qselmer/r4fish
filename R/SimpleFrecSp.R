
#' Plot: Estructura de tallas para "anchoveta "spTarget" lances
#'
#' @param data datos del crucero en formato MF
#' @param sp especie objetivo de la evaluación
#' @param stock  stock objetivo de la evaluación
#' @param save valor logico; ¿Se deben guardar la matriz de lances en "csv"?
#'
#' @return
#' @export
#'
#' @examples
#'
SimpleFrecSp <- function(data = tmp,
                         sp = "anchoveta",
                         stock = "nc",
                         save = T){

  require(ruisu)

# -------------------------------------------------------------------------

name = trimws(data[1, "crucero"])

if(is.na(stock)){
  file <- paste("SimpleFrecSets", name, sp, sep = "_")
  }else{
    file <- paste("SimpleFrecSets", name, sp, stock, sep = "_")
    }

name = file.path(cout, file)

marks0 <- r4fish:::.getMarks(sp = sp, stock = stock)
rmrk <- range(marks0)
rbin <- r4fish:::.getSp(sp = sp, stock = stock)$bin.l
range_lat <- r4fish:::.getLati(sp = sp, stock = stock)
juv0 <- r4fish:::.getJuv(sp = sp, stock = stock)
marks <- data.frame(l = marks0)

data[, "sp"] <- tolower(data[, "sp"])

data[, "buque"] <- trimws(data[, "buque"])
data[, "lance"] <- trimws(data[, "lance"])
data <- data[range_lat[1] < data$lat & range_lat[2] >= data$lat,]
data <- data[data$sp == sp, ]
data <- data[!is.na(data$frec), ]

data$vessel_set = paste(data$buque, data$lance, sep = "-")
listVessel <- unique(data$vessel_set)
vectorRes <- NULL; vectorSet <- NULL

for(r in seq_along(listVessel)){
  tmp <- data[data$vessel_set  == listVessel[r],]
  tmp <- tmp[,c("crucero", "buque","lance","lon","lat",
                "l","frec", "captura")]
  vectorLance <- c(tmp$crucero[1], tmp$buque[1], tmp$lance[1])
  distC <-  distCoast(lon = tmp$lon[1], lat = tmp$lat[1])
  vectorCoor <- c(tmp$captura[1],tmp$lon[1],tmp$lat[1], distC)
  tmp <- merge(tmp, marks, all = T)
  tmp$frec[is.na(tmp$frec)] <- 0
  vectorFreq <- t(tmp$frec)
  vectorOut <- c(vectorCoor, vectorFreq)
  vectorRes <- rbind(vectorRes, vectorOut)
  vectorSet <- rbind(vectorSet,vectorLance)
}

vectorRes <- as.data.frame.array(vectorRes)
vectorSet <- as.data.frame.array(vectorSet)
tmp.out <- cbind(vectorSet, vectorRes)

colnames(tmp.out) <- c("crucero", "buque","lance", "captura", "lon", "lat", "dc", marks$l)
rownames(tmp.out) <- NULL
tmp.out <- tmp.out[order(tmp.out$lat, decreasing = T),]

if(save == T){
  nameFile <- paste0(name, ".csv")
  write.csv(x = tmp.out, file = nameFile, row.names = F)
  cat(nameFile)
}

return(tmp.out)
}
