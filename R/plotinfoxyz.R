plot.info.xyz <- function(colx,
                          xDate = TRUE,
                          coly,
                          zView = FALSE,
                          zCex = 10,
                          colz,
                          title = "Length compositions",
                          colr = NA){
  require(stringr)
  par(oma = c(3,3,4,8), mar = c(0,0,0,0))
  if(xDate == T) colx <- date2year(colx)
  ylim = c(0.5, len.uni(coly)+0.5)

  plot(colx, rep(1, length(colx)),
       axes = T, type = "n", ylim = ylim, ylab = "", xlab = "", yaxt = "n",
       yaxs = "i")

  list_c <- unique(coly)

  abline(h = c(0.5, seq_along(list_c)+0.5), lty = 2, col = "gray50", lwd = 1)
  abline(v = axis(1), lty= 2, col = "gray50", lwd = 1)
  axis(1, at = (range(colx)[1]%/%1) : (range(colx)[2]%/%1), labels = F, tck=-0.01)
  mtext(side = 3, text = toupper(title), line = 0.5, cex = 1.5)

  if(is.na(colr)){
    colr <- rainbow(50, alpha = 1)[sample(1:50,
                                          length(list_c),replace = F )]
  }

  if(zView == T){ cex.nn <- colz/max(colz)*zCex
  } else {
    cex.nn <- rep(zCex/4,length(colz))}

  for (e in seq_along(list_c)) {

    tmpx <- colx[coly == list_c[e]]
    tmpz <- cex.nn[coly == list_c[e]]

    points(tmpx, rep(e, length(tmpx)), cex = tmpz, pch = 16,
           col = adjustcolor(colr[e], 1))
    points(tmpx, rep(e, length(tmpx)), cex = tmpz, pch = 1,
           col = adjustcolor(colr[e], 1))
  }

  labels1 =  str_split_i(string =  list_c, "-", 1)
  labels2 =  str_split_i(string =  list_c, "-", 2)

  axis(4,at = seq_along(list_c), labels = labels1, las = 2 )
  xx <- mean((range(colx)[1]%/%1) : (range(colx)[2]%/%1))
  text(x = rep(xx, length(list_c)), y = seq_along(list_c)+0.3,
       labels = labels2, cex = 0.75)

return(invisible())

}
