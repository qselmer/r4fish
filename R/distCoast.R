# -------------------------------------------------------------------------
#' Title
#'
#' @param lon
#' @param lat
#'
#' @return
#' @export
#'
#' @examples
#'
distCoast = function(lon, lat){
  require(rgdal)
  require(sp)
  require(rgeos)
  temp = data.frame(lon = lon, lat = lat)
  posiciones = temp[,c("lon", "lat")]

  #- Convert VMS data to SpatialPolygons
  spTa              = SpatialPoints(data.frame(posiciones))
  proj4string(spTa) = CRS("+proj=longlat")
  spTa.proj         = spTransform(spTa, CRS("+proj=utm +zone=18 ellips=WGS84"))

  #- Read shapefile of Peru
  polygon_Coast <- PER_ADM0@polygons[[1]]@Polygons[[22]]
  Peru = SpatialPolygons(list(Polygons(list(polygon_Coast), "polygon_Coast")))
  # Peru              = as(PER_ADM0, "SpatialPolygons")
  proj4string(Peru) = CRS("+proj=longlat")
  Peru.proj         = spTransform(Peru, CRS("+proj=utm +zone=18 ellips=WGS84"))
  dists = gDistance(spgeom1 = spTa.proj, spgeom2=Peru.proj,byid=T) #
  distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas

  return(distance)}
