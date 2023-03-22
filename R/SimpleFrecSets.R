
#' Plot: Estructura de tallas para "anchoveta "spTarget" lances
#'
#' @param data datos del crucero en formato MF
#' @param sp especie objetivo de la evaluación
#' @param stock  stock objetivo de la evaluación
#' @param col.sp color de la estructura de tallas en el plot matriz
#' @param cout vector de caracteres que contienen la ruta de salida
#' @param format formato de salida para el plot matriz
#' @param npar dos valores numéricos que definen el numero de filas y columnas
#' para plot matriz "c(x,y)"
#' @param show.warnings valor logico; ¿Se deben resaltar los lances con
#' tamaño de muestreo menor al n.warnings?
#' @param col.warnings color para resaltar los lances warnings
#' @param n.warnings valor numérico que define el umbral para los lances warnings
#' @param ylim dos valores numéricos, que especifican el límite en el eje y
#' @param width
#' @param height
#' @param save valor logico; ¿Se deben guardar la matriz de lances en "csv"?
#'
#' @return
#' @export
#'
#' @examples
#' #' \code{SimpleFrecSets(data = data, sp = "jurel", stock = NA,
#' col.sp = "green", cout = "outputs", format = ".png", npar = c(5,8),
#' show.warnings = T, col.warnings = "gray70", n.warnings = 20,
#' ylim = c(0,0.70), width = 800, height = 900, save = T)
#'
SimpleFrecSets <- function(data = tmp,
                           sp = "anchoveta",
                           stock = "nc",
                           col.sp = "red",
                           cout = "outputs",
                           format = ".pdf",
                           npar = c(5,8),
                           show.warnings = T,
                           col.warnings = "gray70",
                           n.warnings = 20,
                           ylim = c(0,0.60),
                           width = 2625,
                           height = 1750,
                           save = T){

  require(ruisu)

# -------------------------------------------------------------------------
name = trimws(data[1, "crucero"])
name = paste0(cout,"/",name, "_SimpleFrecSets")

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
  tmp <- tmp[,c("buque","lance","lon","lat",
                "l","frec", "captura")]
  vectorLance <- c(tmp$buque[1], tmp$lance[1])
  vectorCoor <- c(tmp$captura[1],tmp$lon[1],tmp$lat[1])
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

colnames(tmp.out) <- c("buque","lance", "captura", "lon", "lat", marks$l)
rownames(tmp.out) <- NULL
tmp.out <- tmp.out[order(tmp.out$lat, decreasing = T),]

if(save == T){
  nameFile <- paste0(name, ".csv")
  write.csv(x = tmp.out, file = nameFile, row.names = F)
}

buque2 <- ifelse(nchar(tmp.out$buque)>15, abbreviate(tmp.out$buque, 10), tmp.out$buque)
vesselSet = paste(buque2, trimws(tmp.out$lance), sep = "-")
area = isopArea.assigner(dataPoints = tmp.out, colLon = "lon", colLat = "lat")
listSets = unique(vesselSet)
numSets = length(listSets)

grad <- floor(-1*tmp.out$lat)
minu <- round((-1*tmp.out$lat)%%1*60)
grad <- ifelse(grad<10, paste0(0, grad), as.character(grad))
minu <- ifelse(minu<10, paste0(0, minu), as.character(minu))
minu <- ifelse(minu == "60","59",minu )

dc <- distCoast(lon = tmp.out$lon, lat = tmp.out$lat)

if(format == ".png"){
  init = seq(1, numSets, npar[1]*npar[2])
  pnglist = list()
  for(x in 1:length(init)){
    if(x == length(init)){
      pnglist[[x]] = seq(init[x], numSets)
    }else{
      pnglist[[x]] = seq(init[x], (init[x]+((npar[2]*npar[1])-1)))
    }
  }
}
if(format == ".png"){

  for (p in 1:length(pnglist)){

    listseq <- pnglist[[p]]

    nameplot = paste0(name,paste0("_", p) ,format)
    png(nameplot, width = width, height = height, res = 250)

    nr <- npar[1]
    nc <- npar[2]

    par(mfrow = c(nr,nc), mar = c(0,0,0,0), oma = c(4.5,4,1,1))

    for(i in listseq){

      nwarnings <- 0
      BaseBySet = tmp.out[i, ]
      x = marks0
      y = unlist(BaseBySet[, ac(marks0)])
      sumy <- sum(y, na.rm = T)
      y = (y/sum(y, na.rm = T))


      if(sumy < n.warnings & show.warnings == T){
        nwarnings <- 1
        cat(paste0("\n","# Posibles lances a filtar:"), fill = TRUE)
        cat(paste0("\n", vesselSet[i]))
      }

      y0 = cleanZeros(y,1)
      juv <- round(100*sum(y[x<juv0], na.rm = T), digits = 1)

      plot(x, y0, type = "n", axes = FALSE, xlab = "", ylab = "",
           xlim = range(x), ylim = ylim,
           xaxs = "i", yaxs = "i", las = 2)
      if(nwarnings == 1){
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
             col = col.warnings, border = col.warnings)
      }
      lines(x, y0, col = col.sp, lwd = 2)

      if(i%%(nr*nc)>((nr-1)*nc) | i%%(nr*nc)==0 | i > (numSets-nc)){
        rug(x =  seq(rmrk[1],rmrk[2],rbin), ticksize = -0.01, side = 1)
        rug(x =  seq(rmrk[1],rmrk[2],rbin*2), ticksize = -0.03, side = 1)
        axis(1, at = seq(rmrk[1],rmrk[2],rbin*4),
             labels = seq(rmrk[1],rmrk[2],rbin*4),
             padj = -1, cex.axis = 0.60)
      }
      if(i%%nc == 1){
        axis(2, las = 2, cex.axis = 0.70)
      }

      abline(v = juv0, lty = 2, col = "gray10")
      mtext(vesselSet[i], side = 3, adj = 0.95, line = -1.5, font = 2,
            cex = 0.50)
      mtext(paste0(" juv=", round(juv), "%"), side = 3, adj = 0.05, line = -4.5,
            cex = 0.60)
      mtext(paste0("n=", sumy), side = 3, adj = 0.05, line = -3.0, cex = 0.60)

      mtext(paste0(grad[i],"°", minu[i], "\' S"), side = 3, adj = 0.95,
            line = -3.0, cex = 0.60)
      mtext(paste0(round(dc[i],1), " mn"), side = 3, adj = 0.95, line = -4.5,
            cex = 0.60)
      box()

      if(i  == listseq[length(listseq)]){
        fy = par("fig")[3]+1
        fx = grconvertY(0, from = "npc", to = "lines") - par("oma")[1]
        mtext(text = "Frecuencia (%)", side = 2, outer = TRUE, line = 2.75, at = fy/2)
        mtext(text = "Longitud total (cm)", side = 1, outer = TRUE, line = 3-fx)
      }

    }
    dev.off()
  }
}
if(format == ".pdf"){

  nameplot = paste0(name, format)
  pdf(nameplot, width = width/300, height = height/300)

  nr <- npar[1]
  nc <- npar[2]
  par(mfrow = c(nr,nc), mar = c(0,0,0,0), oma = c(4.5,4,1,1))

  for(i in 1:numSets){
    nwarnings <- 0
    BaseBySet = tmp.out[i, ]
    x = marks0
    y = unlist(BaseBySet[, ac(marks0)])
    sumy <- sum(y, na.rm = T)
    y = (y/sum(y, na.rm = T))

    if(sumy < n.warnings & show.warnings == T){
      nwarnings <- 1
      cat(paste0("\n","# Posibles lances a filtar:"), fill = TRUE)
      cat(paste0("\n", vesselSet[i]))
    }

    y0 = cleanZeros(y,nzeros = 1)
    juv <- round(100*sum(y[x<juv0], na.rm = T), digits = 1)

    plot(x, y0, type = "n", axes = FALSE, xlab = "", ylab = "",
         xlim = range(x), ylim = ylim,
         xaxs = "i", yaxs = "i", las = 2)
    if(nwarnings == 1){
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
           col = col.warnings, border = col.warnings)
    }
    lines(x, y0, col = col.sp, lwd = 2)

    if(i%%(nr*nc)>((nr-1)*nc) | i%%(nr*nc)==0 | i > (numSets-nc)){
      rug(x =  seq(rmrk[1],rmrk[2],rbin), ticksize = -0.01, side = 1)
      rug(x =  seq(rmrk[1],rmrk[2],rbin*2), ticksize = -0.03, side = 1)
      axis(1, at = seq(rmrk[1],rmrk[2],rbin*4),
           labels = seq(rmrk[1],rmrk[2],rbin*4),
           padj = -1, cex.axis = 0.60)
    }
    if(i%%nc == 1){
      axis(2, las = 2, cex.axis = 0.70)
    }

    abline(v = juv0, lty = 2, col = "gray10")
    mtext(vesselSet[i], side = 3, adj = 0.95, line = -1.5, font = 2,
          cex = 0.50)
    mtext(paste0(" juv=", round(juv), "%"), side = 3, adj = 0.05, line = -4.5,
          cex = 0.60)
    mtext(paste0("n=", sumy), side = 3, adj = 0.05, line = -3.0, cex = 0.60)

    mtext(paste0(grad[i],"°", minu[i], "\' S"), side = 3, adj = 0.95,
          line = -3.0, cex = 0.60)
    mtext(paste0(round(dc[i],1), " mn"), side = 3, adj = 0.95, line = -4.5,
          cex = 0.60)
    box()

    if(i%%(nc*nr)  == 0 | i == numSets){
      fy = par("fig")[3]+1
      fx = grconvertY(0, from = "npc", to = "lines") - par("oma")[1]
      mtext(text = "Frecuencia (%)", side = 2, outer = TRUE, line = 2.75, at = fy/2)
      mtext(text = "Longitud total (cm)", side = 1, outer = TRUE, line = 3-fx)
    }

  }
  dev.off()
  pdf.options(reset = TRUE)
}

return(tmp.out)
cat(nameFile)
}
