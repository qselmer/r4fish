#' Title
#'
#' @param tmp2l
#' @param sp
#' @param stock
#' @param col.sp
#' @param cout
#' @param format
#' @param ylim
#' @param save
#' @param width
#' @param height
#' @param colset
#'
#' @return
#' @export
#'
#' @examples
PlotSimpleFrec2 <- function(data,
                            sp = sp,
                            stock = stock,
                            col.sp = "red",
                            colset = "navajowhite",
                            cout = ".",
                            save = T,
                            format = ".png",
                            ylim = c(0,0.5),
                            width = 2625,
                            height = 1750){
  require(lattice)
  # -------------------------------------------------------------------------
  tmp.out <- tmp2l
  name = trimws(tmp.out[1, "crucero"])

  if(is.na(stock)){
    file <- paste("Plot2SimpleFrec",name, sp, sep = "_")
  }else{
    file <- paste("Plot2SimpleFrec",name, sp, stock, sep = "_")
  }

  nameplot = paste0(file,format)
  nameplot <- file.path(cout, nameplot)


  numSets <- nrow(tmp.out)
  buque2 <- ifelse(nchar(tmp.out$buque)>15, abbreviate(tmp.out$buque, 10), tmp.out$buque)
  listSets<- paste(buque2, trimws(tmp.out$lance), sep = "-")
  marks0 <- r4fish:::.getMarks(sp = sp, stock = stock)
  rmrk <- range(marks0)
  rbin <- r4fish:::.getSp(sp = sp, stock = stock)$bin.l
  juv0 <- r4fish:::.getJuv(sp = sp, stock = stock)

  tmp.out2 <- NULL
  for(u in 1:numSets){
    lx <- tmp.out[u, ]
    rel <- normalize(lx[, as.character(marks0)])
    rel <- unlist(cleanZeros(rel, 1))
    df.out <- data.frame(vessel_set =  listSets[u], marks = marks0, freq = rel)
    tmp.out2 <- rbind(tmp.out2, df.out)
    # print(listSets[u])
  }

  tmpjuv <- tmp.out[, as.character(marks0)]
  tmpjuv.temp <- tmpjuv[, marks0 < juv0]
  nx <- rowSums(tmpjuv)
  nxjuv <- round((rowSums(tmpjuv.temp)/ nx)*100,0)
  nxjuv <- paste0(" juv=", round(nxjuv), "%")

  dc <- round(tmp.out$dc, 0)

  grad <- floor(-1*tmp.out$lat)
  minu <- round((-1*tmp.out$lat)%%1*60)
  grad <- ifelse(grad<10, paste0(0, grad), as.character(grad))
  minu <- ifelse(minu<10, paste0(0, minu), as.character(minu))
  minu <- ifelse(minu == "60","59",minu )
  lati <- paste0(grad,"°", minu, "\' S")

  ylast <- r4fish::last(x = marks0)-rbin
  tmp.out2$vessel_set <- factor(tmp.out2$vessel_set, levels = listSets)

  mypanel = function(x, y,...) {
    panel.xyplot(x, y, type = "l",col = col.sp, lwd =2)
    panel.abline(v = juv0, col = 1, lwd = 1, lty = 2)

    panel.text(ylast, ylim[2]-(0.1*1), lati[panel.number()],
               cex = 0.65, pos =2, font = 2 )
    panel.text(ylast, ylim[2]-(0.1*1.5), paste0("\n",dc[panel.number()], " mn"),
               cex = 0.65, pos =2,  font = 2 )
    panel.text(ylast, ylim[2]-(0.1*3), paste0("\nn = ", nx[panel.number()]),
               cex = 0.65, pos = 2)
    panel.text(ylast, ylim[2]-(0.1*4.5), nxjuv[panel.number()],
               cex = 0.65, pos = 2)
  }

  tp.plot <- xyplot(freq~marks|vessel_set, data= tmp.out2,
                    panel = mypanel,
                    ylab='Frecuencia (%)',
                    xlab='Longitud total (cm)',
                    ylim = ylim,  as.table=TRUE,
                    xaxs = "i", yaxs = "i", las = 2,
                    par.settings = list(strip.background=list(col= colset),
                             axis.text = list(cex = 1),
                             par.xlab.text = list(cex = 1),
                             par.ylab.text = list(cex = 1),
                             par.main.text = list(cex = 1)),
                    strip = strip.custom(bg= colset,
                              par.strip.text=list(col="black",
                                                  cex=.8,
                                                  font=3)),
                    scales=list(cex=0.8,
                     tck=c(0.5,0.5), x=list(cex=0.8), y=list(cex=0.8))
                    )

  plot(tp.plot)

  if(save == T){
    if(format == ".png"){
      dev.copy(png, filename = nameplot,  width = width, height = height, res = 180)
      dev.off()
    }
    if(format == ".pdf"){
      dev.copy(pdf,  nameplot,  width = width/200, height = height/200)
      dev.off()
    }
  }

  return(invisible())
}


