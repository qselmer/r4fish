addAIP = function(border =  "khaki1", col = NA,  ylim){
  
  # dataIsopara = read.csv("areas-isoparalitorales.csv")
  dataAIP <- r4fish::AIP_coordenadas
  dataIsopara = subset(dataAIP, subset = dataAIP[, "lat"] <= ylim[2] & dataAIP[, "lat"] >= ylim[1])
  # dataIsopara = subset(dataAIP, subset = dataAIP[, "lon"] <= xlim[2] & dataAIP[, "lon"] >= xlim[1])
  
  idx_areas = unique(dataAIP$area)
  
  for(r in seq_along(idx_areas)){
    
    temp = subset(x = dataAIP, subset = dataIsopara$area == idx_areas[r])
    polygon(x = c(temp$lon[1], temp$lon[2:nrow(temp)], temp$lon[1]),
            y = c(temp$lat[1], temp$lat[2:nrow(temp)], temp$lat[1]),
            border = border, col = col)
    
  }
  
  nAIP <- paste("n AIP", length(idx_areas))
  print(nAIP)
  return(invisible())
}
