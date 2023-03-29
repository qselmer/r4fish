#' Title
#'
#' @param dat
#' @param sp
#' @param src
#' @param type
#' @param ab
#' @param factor
#' @param cexf
#' @param col.in
#' @param col.bd
#' @param marp
#' @param omap
#' @param mgpp
#' @param widthFig
#' @param heightFig
#' @param resFig
#' @param SavePlot
#' @param dirout
#'
#' @return
#' @export
#'
#' @examples
plot2CompSizeTime <- function(dat = dat_marks,
                              sp = "anchoveta",
                              stock = "nc",
                              type = "B",
                              ab = NA,
                              factor = NA,

                              cexf = 3,
                              col.in = "gray90",
                              col.bd = "black",

                              marp = c(3,3,2,2),
                              omap = c(1,2,1,1),
                              mgpp =c(1,0.5,0),

                              widthFig = 2600,
                              heightFig = 3200,
                              resFig = 380,
                              SavePlot = T,
                              dirout = "Outputs/"
                              ){
  spT <- r4fish:::.getSp(sp = sp, stock = stock)
  marks <- r4fish:::.getMarks(sp = sp, stock = stock)
  mjuv <- r4fish:::.getJuv(sp = sp, stock = stock)


  if(is.na(ab)){ ab <- list(a = spT$a, b = spT$b) }
  peso <- ab$a*(marks^ab$b)

  mat <- dat[, -1]
  if(type == "B"){
    mat <- sweep(x = dat[, -1], MARGIN = 2, STATS = peso, FUN = "*")
  }

  time <- dat[,1]

  if(ncol(mat) != length(marks)){
    errorCondition("The length of the marks are not the same")
  }

  line_t <- seq(floor(range(time)[1]), ceiling(range(time)[2]), 0.25)
  line_t1 <- seq((range(time)[1]), (range(time)[2]), 0.5)
  line_t2 <- seq(floor(range(time)[1]), floor(range(time)[2]), 1)

  mat_abs <- mat/max(mat, na.rm = T)*cexf

  par(mgp= mgpp, mar =  marp, oma = omap)

  tmp_yf <- matrix(t(as.matrix(time)), nrow = length(time), ncol = length(marks))
  tmp_yf <- min(tmp_yf - mat_abs)

  plot(x = marks, y = rep(1, length(marks)), type = "n", col = 2,
       ylim = c(rev(range(time))[1], tmp_yf), xaxt = "n",
       xlab = "", ylab = "", yaxt = "n",xaxs="i", yaxs="i", bty='l')

  axis(side = 2, line_t1 , tck= -0.01, labels= FALSE)
  axis(side = 2, line_t2, tick = FALSE, labels= line_t2, las = 2, cex.axis = 0.9)

  axis(1,  marks, labels =  marks, las = 1, cex.axis = 0.9)


  for(i in 1:length(time)){

    timef <- time[i]
    tmp  <- mat_abs[i, ]
    tmp_y <- timef - tmp

    porj <- mat[i, ]
    porj <- (sum(porj[marks < mjuv])/ sum(porj))*100
    porj <- round(porj, digits = 0)

    abline(h = timef)
    polygon(c(marks, rev(marks)),c(rep(timef, length(marks)), rev(tmp_y)),
            col = col.in, border = col.bd, lwd = 1)

    text(x = 18 , y = timef-0.25, labels = paste0(porj, "%"),
         cex = 0.65, adj = -0.1)
  }

  abline(v = mjuv, lty = 2, col = "red", lwd = 1.2)

  mtext("Año", side = 2, line = 3, cex = 1)
  mtext(spT$lengthType, side = 1, line = 2.5, cex = 1)

  corners = par("usr")
  op <- par(xpd = TRUE)
  text(x = corners[2]+0.8, y = mean(corners[3:4]), "Abundancia", srt = 270)
  par(op)

  if(SavePlot == T){
    lb1 <- ifelse(type == "N", "Abundancia", "Biomasa")
    fileName = paste(lb1, spT$acustica, src,
                     round(floor(time)[1], 0), round(floor(time)[length(time)], 0),
                     sep =  "-")
    dev.copy(png,
             filename = file.path(dirout, paste0(fileName,".png")),
             width = widthFig, height = heightFig, res = resFig)
    dev.off()
  }

  return(invisible())

}
