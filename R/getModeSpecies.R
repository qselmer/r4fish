#' Title
#'
#' @param Length
#' @param sp
#' @param src
#' @param nmodes
#' @param tol
#' @param savePlot
#' @param dirout
#'
#' @return
#' @export
#'
#' @examples
getModeSpecies <- function(Length = Length,
                           sp = "anchoveta",
                           src = "fsh",
                           nmodes = 2,
                           tol = 0,
                           savePlot = TRUE,
                           dirout = "Outputs/"
                           ){

  sp <- getSpeciesInfo(sp = sp)
  if(src == "fsh"){len <- seq(sp$Lmin.fsh,sp$Lmax.fsh, sp$bin.L)}
  if(src == "pope"){len <- seq(sp$Lmin.pope, sp$Lmax.pope, sp$bin.L)}
  modaL <- NULL
  modaF <- NULL

  if(savePlot == TRUE){
    pdf(file = file.path(dirout, paste0(paste(sp, src, "modas-Plot", sep = "-"), ".pdf")), width = 6*1.5, height = 4*1.5)
    par(mfrow = c(5, ceiling(nrow(Length)/10)), oma =c(2,3,2,2), mar = c(0,0,0,0))
  }

  for (i in 1:length(Length[, 1])) {

    freq <- as.numeric(Length[i, ])
    if(savePlot == TRUE){
      plot(len, freq, type = "n", lwd = 2, las = 2, xaxt = "n", yaxt = "n" )
      grid()
      lines(len, freq, lwd = 2)
    }
    tempdat <- data.frame(l = len, frec = 0)
    temp2 <- data.frame(l = len, frec = freq)
    tmp3 <- merge(x = tempdat, y = temp2, by = "l", all.x = T)
    outDat <- data.frame(l = tempdat$l, frec = rowSums(tmp3[,2:3], na.rm = T))
    out <- rep(NA, nmodes)
    out3 <- rep(NA, nmodes)
    idmoda <- NULL

    for(j in 1:nrow(outDat)){

      # print(j)
      if(j != 1) val1 = outDat$frec[j-1]
      valmid = outDat$frec[j]
      if(j != nrow(outDat)) val2 = outDat$frec[j+1]

      if(valmid != 0){

        if(j == 1){
          if(valmid > val2){idmoda = c(j, idmoda)}
          if(valmid == val2){idmoda = c(j+1, idmoda)}
        }

        if(j == nrow(outDat)){
          if(valmid > val1){idmoda= c(j, idmoda)}
          if(valmid == val1){idmoda = c(j-1, idmoda)}
        }

        if(j != nrow(outDat) & j != 1){

          if(valmid > val1 & valmid > val2){idmoda = c(j, idmoda)}
          if(valmid <= val1 & valmid <= val2){idmoda = idmoda}
          if(valmid > val1 & valmid == val2){
            if(sum(valmid,  val1) > sum(val2, outDat$frec[j+2], na.rm = T)){idmoda = c(j, idmoda)}
            if(sum(valmid,  val1) < sum(val2, outDat$frec[j+2], na.rm = T)){idmoda = c(j+1, idmoda)}
            if(sum(valmid,  val1) == sum(val2, outDat$frec[j+2], na.rm = T)){idmoda = idmoda}
          }
          if(valmid == val1 & valmid > val2){
            if(sum(outDat$frec[j-2],  val1) > sum(valmid, val2, na.rm = T)){idmoda = c(j-1, idmoda)}
            if(sum(outDat$frec[j-2],  val1) < sum(valmid, val2, na.rm = T)){idmoda = c(j, idmoda)}
            if(sum(outDat$frec[j-2],  val1) == sum(valmid, val2, na.rm = T)){idmoda = idmoda}
          }
        }
      }

      # outDat <- outDat[idmoda, ]
    }

    idmoda <- unique(idmoda)
    idmoda <- idmoda[which(outDat$frec[idmoda] > tol)]
    idOrder <- order(outDat$frec[idmoda], decreasing = T)
    modaLeng <- outDat$l[idmoda][idOrder]
    modaFreq <- outDat$frec[idmoda][idOrder]
    if(savePlot == TRUE){
      points(modaLeng, modaFreq, col = adjustcolor(2, 0.5), cex = 2, pch = 16)
      abline(v  = 12, lty = 2, col = 2)
    }
    modaLeng <- modaLeng[1: nmodes]
    modaFreq <- modaFreq[1: nmodes]

    modaF <- rbind(modaF, modaFreq)
    modaL <- rbind(modaL, modaLeng)
  }

  if(savePlot == TRUE){dev.off()}


  row.names(modaL) <- NULL
  row.names(modaF) <- NULL

  out <- list(modaLeng = modaL , modaFreq = modaF)
  return(out)
}
