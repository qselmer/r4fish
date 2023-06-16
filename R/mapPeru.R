mapPeru <- function (xlim = c(-86, -70), ylim = c(-21, -3), xlab = "",
                     ylab = "", col.map = "gray80", border.map = "gray10",
                     nprof = 2, space.prof = 3, dprt = T, area.iso = FALSE,
                     harbor = 1, cex.harbor = 1, col.harbor = 1, cex.axis = 1,
                     col.sea = NA){
  require(sp)
  axis.Lon <- paste(abs(seq(xlim[1], xlim[2], by = 2)), "°W")
  axis.Lat <- paste(abs(seq(ylim[1], ylim[2], by = 2)), "°S")
  Encoding(axis.Lon) <- "UTF-8"
  Encoding(axis.Lat) <- "UTF-8"

  xlim2 <- xlim
  if (nprof > 1) {
    xlim2[1] <- xlim2[1] + (nprof - 1) * (-space.prof)
  }

  plot(1, xlim = xlim2, ylim = ylim, axes = FALSE, xlab = xlab,
       ylab = ylab)

  if(!is.na(col.sea)){
    #lightblue
    x_limits <- par("usr")[1:2]
    y_limits <- par("usr")[3:4]
    rect(x_limits[1], y_limits[1], x_limits[2], y_limits[2],
         col = col.sea)
  }

  if (isTRUE(area.iso)) {
    infIso <- r4fish::Grid_isoparalitoral_sf
    plot(x = infIso, add = T, col = NA, border = "lightgray")
    }

  linePeru <- r4fish::map_coastline
  polygon(x = c(linePeru$lon[1], -50, -50, linePeru$lon[23513:2], linePeru$lon[1]),
          y = c(linePeru$lat[1], -24, 0, linePeru$lat[23513:2], linePeru$lat[1]),
          col = col.map, border = border.map)

  if(isTRUE(dprt)){
    map_peru <- r4fish::map_peru
    plot(map_peru, add = T, col = col.map, border = border.map)
  }

  if (nprof > 1) {
    for (i in 2:nprof) {
      lines(linePeru$lon + (i - 1) * -space.prof, linePeru$lat,
            col = border.map)
    }
  }

  puertosPeru <- r4fish::map_harbordata
  idx <- which(puertosPeru[, paste0("shw.", harbor)] == 1)
  puertosPeru <- puertosPeru[idx, ]
  text(puertosPeru$lon, puertosPeru$lat, labels = puertosPeru$name,
       pos = 4, col = col.harbor, cex = cex.harbor)

  axis(2, seq(ylim[1], ylim[2], by = 1), labels = F, las = 1,
       cex.axis = cex.axis, hadj = 0.5, tck = -0.01)
  axis(2, seq(ylim[1], ylim[2], by = 2), axis.Lat, las = 1,
       cex.axis = cex.axis, hadj = 0.5, tck = -0.01)

  if (nprof == 1) {
    axis(1, seq(xlim[1], xlim[2], by = 1), tck = -0.01,
         labels = NA, hadj = 0.5)
    axis(1, seq(xlim[1], xlim[2], by = 2), labels = axis.Lon,
         cex.axis = cex.axis, line = -0.8, lwd = 0)
  }

  box()
  return(invisible())
}
