MapEchosAcustics <- function(data, sp = sp, stock = stock, 
                             cout = ".", xlim = c(-83, -70), ylim = c(-20, -3),
                             pch = 16 , col.point = 1, cex.point = 0.9, 
                             width.fig = 700, height.fig = 820, format = ".png",
                             by.vessel = TRUE, npro = 3, by.pro = ".",  
                             echo.size = TRUE, save = FALSE, port.import = 1, 
                             add.AIP = T, legend = TRUE, 
                             lat.tck = 2, lot.tck = 2, 
                             col.inland = "khaki1", alpha.col = 1){
  
  names(data) = tolower(names(data))
  spCol <- r4fish:::.getSp(sp = sp, stock = stock)$acustic    
  lat <- r4fish:::.getLati(sp = sp, stock = stock)
  data <- subset(data, lat_m > lat[1] & lat_m <= lat[2] )
  
  if(is.element("cruise", colnames(data))){
    name <- paste0(unique(data[, c("cruise")]), format)
    }else{
      name <- paste0("MapEchosAcustics", format)
      }
  
  filename = paste0(name, format)
  
  if(save){
    switch(format, .png = png(filename = file.path(cout, name), width = width.fig,
                              height = height.fig, res = 180),
           .tiff = tiff(filename = file.path(cout, name), width = width.fig,
                       height = height.fig, res = 180),
           .pdf = pdf(file = file.path(cout, name) ,width = width.fig, 
                      height = height.fig))
    }
  
  par(mar = c(3,3,1,1))
  plot(NA, NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "", xaxs = "i",
         yaxs = "i")
  if(add.AIP){ addAIP(border = "red", col = NA, ylim = ylim) }
  
  if(!is.element(spCol, colnames(data))){stop("acoustic name must be in the data")}
  if(echo.size){
    data$cex.point2 = (data[ , spCol]/max(data[ , spCol], na.rm = T))*cex.point
    }else{
      data$cex.point2 = cex.point
      }
  
  if(is.element("vessel", colnames(data))){
    if(by.vessel){
      vessel <- unique(data[, "vessel"])
      vessel2 <- tolower(vessel)
      cols.v <-  rainbow(length(vessel2), alpha = alpha.col)
      for(k in seq_along(vessel)){
        points(x = data[which(data[, "vessel"] == vessel[k]), "lon_m"],
               y = data[which(data[, "vessel"] == vessel[k]), "lat_m"],
               pch = pch, col = cols.v, 
               cex = data[which(data[, "vessel"] == vessel[k]), "cex.point2"])
      }
    }
    
    if(legend) legend("bottomleft", legend = vessel2, pch = NA, bty = "n", 
                      col = cols.v, cex = 0.9, text.col = cols.v)
  }else{
    points(x = data[ ,"lon_m"], y = data[ ,"lat_m"],
           col = adjustcolor(col.point, alpha.f = alpha.col), 
           pch = pch, cex = data$cex.point2)
  }
  
  # ------------------------------
  latvec <- seq(-21, -3, by = lat.tck)
  lonvec <- seq(-84, -3, by = lot.tck)
  linePeru <- r4fish::map_coastline
  lines(linePeru$lon, linePeru$lat, col = 1)
  polygon(x = c(linePeru$lon[1], -50, -50, linePeru$lon[nrow(linePeru):2], linePeru$lon[1]), 
          y = c(linePeru$lat[1], -24, 0, linePeru$lat[nrow(linePeru):2], linePeru$lat[1]), 
          col = col.inland)
  
  axis(1, at = lonvec, labels = paste0(abs(lonvec), "°W"), las = 2, cex.axis = 0.9)
  axis(1, at = seq(-84, -70, by = 1), labels = NA)
  axis(2, at = latvec, labels = paste0(abs(latvec), "° S"), las = 2, cex.axis = 0.9)
  axis(2, at = seq(-21, -3, by = 1), labels = NA)
    
  puertos <- r4fish::map_harbordata
  if(port.import == 1){
    harbor = puertos[puertos$importance == 1,]
    } else {
      harbor = harbor
    }
  
  text(x = harbor$lon, y = harbor$lat, labels = harbor$name, cex = 0.8, pos = 4)
  box()
  
  if(is.element("cruise", colnames(data))){
    labely <- unique(data[, "cruise" ])
    labely <- strsplit(labely, " ")[[1]]
    nl <- length(labely) %/% 2 
    labely1 <- paste(labely[1: nl], collapse = " ")
    labely2 <- paste(labely[(nl+1): length(labely)], collapse = " ")
    mtext(text = labely1, side = 3, line = -1, adj = 0.9, cex = 0.5)
    mtext(text = labely2, side = 3, line = -1.5, adj = 0.9, cex = 0.5)
  }
  
  if(save){
  while (dev.cur()>1) dev.off()
  }
  
  return(invisible())

}