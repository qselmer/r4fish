mapPeru <- function (xlim = c(-86, -70), ylim = c(-21, -3), xlab = "",
                     ylab = "", col.map = "khaki1", border.map = "gray10",
                     nprof = 2, space.prof = 3, dprt = T, grid = NA,
                     harbor = 1, cex.harbor = 0.6, col.harbor = 1, cex.axis = 1,
                     col.sea = F, title = "", compass =F){
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

  if(isTRUE(col.sea)){
    #lightblue
    x_limits <- par("usr")[1:2]
    y_limits <- par("usr")[3:4]
    rect(x_limits[1], y_limits[1], x_limits[2], y_limits[2],
         col = "lightblue")
  }

  if (!is.na(grid)) {
    if(grid == "onedegree"){
      shapefile <- r4fish::Grid_onedegree_A_sf
    }else{
      if(grid == "halfdegree"){
        shapefile <- r4fish::Grid_halfdegree_B_sf
      }else{
        if(grid == "quarterdegree"){
          shapefile <- r4fish::Grid_quarterdegree_C_sf
        }else{
          if(grid == "isoparalitoral"){
            shapefile <- r4fish::Grid_isoparalitoral_sf
          }
        }
      }
    }
    plot(shapefile, add = T, col = NA, border = "gray75")
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

  mtext(text = title, side = 3, line = -1.5, adj = 0.95)

  if(compass){
    require(png)
    templ <- system.file("fig", "0compass.png", package = "r4fish")
    img <- readPNG(templ)
    asp <- dim(img)[2] / dim(img)[1]
    xx <- xlim[2] -2
    yy <-  ylim[2] -5
    rasterImage(image = img, xx, yy, xx+asp*1.75,yy+asp*3.75, interpolate = T)

  }
  return(invisible())
}
