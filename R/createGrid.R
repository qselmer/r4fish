

# mapas por 0.5, 1, API --------------------------------------

create.grid <- function(xxlim = c(-86, -70),
                        yylim = c(-21, -3),
                        by.grid = 0.5, cap = NULL,
                        saveRda = T, cout = "."){
  require(r4fish) 
  require(sp) 
  require(sf)
  require(rgdal)

  crs <- st_crs(r4fish::AIPShapefile_new)
  # Definir los límites de la grilla
  limite_grilla <- st_bbox(c(xmin = xxlim[1], ymin = yylim[1], xmax = xxlim[2],
                             ymax = yylim[2]))
  # Calcular el número de celdas en cada dimensión
  xxlon <- ceiling(diff(xxlim) / by.grid)
  yylon <- ceiling(diff(yylim) / by.grid)
  # Crear la grilla con intervalos 
  grid_sf <- st_make_grid(limite_grilla, n = c(xxlon, yylon), what = "polygons")
  # Asignar el sistema de referencia de coordenadas (CRS)
  st_crs(grid_sf) <- crs
  df.coor <- as.data.frame.array(st_coordinates(grid_sf))
  df.coor$key <- paste(df.coor$L1, df.coor$L2, sep = "-")
  listkey <- unique(df.coor$key)
  df.coor$code <- NA
  if(is.null(cap)){cap <- sample(LETTERS, size = 1)}else{cap <- toupper(cap)}
  for(u in seq_along(listkey)){
  df.coor[df.coor$key == listkey[u],]$code <- paste0(cap, u)
  }

  area_m = as.numeric(st_area(grid_sf))
  area_mn = as.numeric(st_area(grid_sf))*3.861021e-07
  centroids <- st_centroid(grid_sf)
  coords <- st_coordinates(centroids)
  x_vals <- coords[, "X"]
  y_vals <- coords[, "Y"]

  grid_df <- data.frame(code = paste0(cap, 1:length(grid_sf)),
                        lon = x_vals, lat = y_vals, area_m = area_m, 
                        area_mn = area_mn)

  grid_sf <- st_as_sf(grid_sf)
  grid_sp <- as(grid_sf, "Spatial")

  # Asignar los datos del marco de datos a SpatialPolygonsDataFrame
  grid_spdf <- SpatialPolygonsDataFrame(grid_sp, data = grid_df)
  grid_spdf <- spTransform(grid_spdf, CRSobj = proj4string(r4fish::AIPShapefile_new))
  
  filename <- paste("grid", paste0(number2text(by.grid), "degree"), sep = "_")
  
  
  if(saveRda){
    #
    save(df.coor, file  = file.path(cout, paste0(filename, "Data", ".rda")))
    save(grid_spdf, file  = file.path(cout, paste0(filename, "Shapefile", ".rda")))
  }
  
  return(grid_spdf)
  
}
  
  

  
    #######
dataPoints <- data.frame(colLon = -85, colLat =  -20.0)
colLon = "colLon"; colLat = "colLat"
dataPoints <- switch(class(dataPoints), matrix = dataPoints[,c(colLon, colLat)], 
                     data.frame = data.frame(lon = dataPoints[,  colLon], lat = dataPoints[, colLat]), 
                     numeric = data.frame(lon = dataPoints[colLon], lat = dataPoints[colLat]))

output <- rep(NA, nrow(dataPoints))
index <- complete.cases(dataPoints)
dataPoints <- dataPoints[index, ]
# ruisu:::assigner_isoparalitoral(dataPoints = dataPoints)
# ruisu:::assigner_marsdenSquare(dataPoints = dataPoints)   



coordinates(dataPoints) <- dataPoints
proj4string(dataPoints) <- proj4string(grid_spdf)
dataPoints <- over(x = dataPoints, y = grid_spdf)

poly.coor <- df.coor[df.coor$code == dataPoints$code,]
plot(1,1, xlim = range(df.coor$X), ylim = range(df.coor$Y))
polygon(x = poly.coor$X, y = poly.coor$Y, col = "red")
dataPoints2 <-  data.frame(colLon = -85, colLat =  -20.0)
points(dataPoints2$colLon, dataPoints2$colLat)
text.default(grid_spdf$lon, grid_spdf$lat, labels  = grid_spdf$code, cex = 0.3)



# Especifica la ruta y el nombre de archivo para guardar el shapefile
shapefile_path <- "ruta/al/archivo/grid_spdf.shp"

# Guarda el objeto grid_spdf como shapefile

