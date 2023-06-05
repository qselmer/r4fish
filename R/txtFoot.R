txtFoot <- function (string = get.version(), cex = 0.5, do.flag = NULL) 
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
