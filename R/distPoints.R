distPoints = function(lonA, latA, lonB, latB){
  temp1 = data.frame(lon = lonA, lat = latA)
  temp2 = data.frame(lon = lonB, lat = latB)
  posiciones1 = temp1[,c("lon", "lat")]
  posiciones2 = temp2[,c("lon", "lat")]
  #- Convert VMS data to SpatialPolygons
  spTa1              = SpatialPoints(data.frame(posiciones1))
  proj4string(spTa1) = CRS("+proj=longlat")
  spTa.proj1         = spTransform(spTa1, CRS("+proj=utm +zone=18 ellips=WGS84"))
  spTa2              = SpatialPoints(data.frame(posiciones2))
  proj4string(spTa2) = CRS("+proj=longlat")
  spTa.proj2         = spTransform(spTa2, CRS("+proj=utm +zone=18 ellips=WGS84"))

  dists = gDistance(spgeom1 = spTa.proj1, spgeom2=spTa.proj2, byid=T) #
  distance       = as.vector(t(dists*0.00053996)) # convirtiendo de metros a millas nauticas
  return(distance)}
