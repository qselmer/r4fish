#' Title
#'
#' @param data
#' @param xlon
#' @param ylat
#' @param by
#' @param metadata
#' @param save
#' @param cout
#'
#' @return
#' @export
#'
#' @examples
assignerPeruvianGrid <- function(data, xlon = "lon",
                                 ylat = "lat", by = "onedegree",
                                 metadata = F, save = T, cout = "."){
  require(sp)
  # by = c("onedegree", "halfdegree", "isopalitoral")
  if(trimws(tolower(by)) == "onedegree"){
    shapefile <- r4fish::Grid_onedegree_A_sf
    info <- r4fish::Grid_onedegree_A_data
  }else{
    if(trimws(tolower(by)) == "halfdegree"){
      shapefile <- r4fish::Grid_halfdegree_B_sf
      info <- r4fish::Grid_halfdegree_B_data
    }else{
      if(trimws(tolower(by)) == "isoparalitoral"){
        shapefile <- r4fish::Grid_isoparalitoral_sf
        info <- r4fish::Grid_isoparalitoral_data
      }else{
        if(trimws(tolower(by)) == "quarterdegree"){
          shapefile <- r4fish::Grid_quarterdegree_C_sf
          info <- r4fish::Grid_quarterdegree_C_data
          }else{
            stop("grid not found")
          }
      }
    }
  }


  coor <- switch(class(data), matrix = data[, c(xlon, ylat)],
                 data.frame = data.frame(lon = data[, xlon], lat = data[, ylat]),
                 numeric = data.frame(lon = data[xlon], lat =  data[ylat]))
  output <- rep(NA, nrow(coor))
  if(any(!complete.cases(coor))) stop("missing coordinates")

  coordinates(coor) <- coor
  proj4string(coor) <- proj4string(shapefile)
  coor2 <- over(x = coor, y = shapefile)
  if(metadata){coor2 <- coor2}else{coor2 <- coor2["code", ]}
  if(save){
    fileName = paste0("assignerPeruvianGrid-",Sys.Date(),".csv")
    write.table(coor2, file.path(cout, fileName), row.names = F, sep = ",")
  }
  coor3 <- list(asignerGrid =coor2, type = by)
  print(coor3$type)
  return(coor3)
}
