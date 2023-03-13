

point_area <- function(x,
                       y,
                       label, 
                       sp = "anchoveta",
                       plot = T, 
                       catg = 1, 
                       col = 1){
  require(sp)
  permAIP <- AIP_borde
  indx <- point.in.polygon(point.x = x,point.y =  y, pol.x = permAIP$x, pol.y = permAIP$y, mode.checked=FALSE)
  if(any(indx == 0)){print("Hay puntos fuera del area")}
  points0 <- label[indx == catg]
  if(any(indx == catg)){
    if(plot == T){
      mapa_peru2(land.col ="gray", border.map = 1, col_harbor = "gray", xlim = c(-110, -65), ylim = c(-29,3))
      polygon(permAIP$x, permAIP$y, pch =16, col = NULL, border = 4, lwd = 1, lty =2)
      points(x[indx == catg], y[indx == catg], pch = 16, col = col, lwd = 3)
      text(x = x[indx == catg],  y =y[indx == catg], labels = points0, 
           col = 4, font =2, cex = 0.7, pos = 2)
    }
  }
  res <- list(point.in.area= indx, points.lances = points0)
  return(res)
}