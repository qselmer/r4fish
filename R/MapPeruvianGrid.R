#' Title
#'
#' @param data
#' @param colcode
#' @param colval
#' @param xxlim
#' @param yylim
#' @param by
#' @param gradient
#' @param border
#' @param all.grid
#' @param land.col
#' @param land.border
#' @param legend
#' @param txtleg
#' @param xaxis
#' @param yaxis
#' @param portImport
#' @param save
#' @param cout
#'
#' @return
#' @export
#'
#' @examples
MapPeruvianGrid <- function(data, colcode = "code" , colval = "freq",
                            typeval = "#",
                            xxlim = c(-86, -70), yylim = c(-21, -3),
                            by = "onedegree",
                            gradient = c("yellow", "red"),
                            border = NA,
                            all.grid = F,
                            land.col = "gray90", land.border = "gray90",
                            legend = T, txtleg = "(n)",
                            xaxis = 1, yaxis = 4,
                            portImport = 2, save = T, cout = "."){
  require(geoR)
  # by = c("onedegree", "halfdegree", "isopalitoral")
  if(trimws(tolower(by)) == "onedegree"){
    shapefile <- r4fish::Grid_onedegree_A_sf
    info <- r4fish::Grid_onedegree_A_data
  }else{
    if(trimws(tolower(by)) == "halfdegree"){
      shapefile <- r4fish::Grid_halfdegree_B_sf
      info <- r4fish::Grid_halfdegree_B_data
    }else{
      if(trimws(tolower(by)) == "isopalitoral"){
        shapefile <- r4fish::Grid_isoparalitoral_sf
        info <- r4fish::Grid_isoparalitoral_data
      }else{
        if(trimws(tolower(by)) == "quarterdegree"){
          shapefile <- r4fish::Grid_quarterdegree_C_sf
          info <- r4fish::Grid_quarterdegree_C_data

        }
        stop("grid not found")
      }
    }
  }

  coor <- switch(class(data), matrix = data[, c(colcode, colval)],
                 data.frame = data.frame(code = data[, colcode], val = data[, colval]),
                 numeric = data.frame(code = data[colcode], val =  data[colval]))

  colfunc <- colorRampPalette(gradient)

  if(typeval == "#"){

    minCols = floor(min(coor[,"val"]))
    maxCols = ceiling(max(coor[,"val"]))
    totCols = (maxCols - minCols)*10 + 1
    PalCols = colfunc(totCols)
    coor$colfunc = round(coor[,"val"],1)*10
    coor$colfunc2 = coor$colfunc - (minCols*10) + 1
  }

  if(typeval == "%"){
    coor[,"val"] = round(coor[,"val"])
    PalCols = colfunc(101)
    coor$colfunc2 = coor[,"val"] + 1
  }

  idx_areas <- unique(info[, "code"])

  plot(1,1, type = "n", ylim = yylim, xlim = xxlim, axes = F,
       xlab = "", ylab = "")

  for(i in seq_along(idx_areas)){
    print(idx_areas[i])
    temp = subset(x = coor, subset = coor$code == idx_areas[i])
    temp2 <- subset(info, subset = info$code == idx_areas[i])
    if(all.grid){border2 = border}else{border2 = NA}

    if(nrow(temp) == 0){
      polygon(x = c(temp2$lon[1], temp2$lon[2:nrow(temp2)], temp2$lon[1]),
              y = c(temp2$lat[1], temp2$lat[2:nrow(temp2)], temp2$lat[1]),
              border = border2, col = 0)
    }

    if(nrow(temp) >  0){
      polygon(x = c(temp2$lon[1], temp2$lon[2:nrow(temp2)], temp2$lon[1]),
              y = c(temp2$lat[1], temp2$lat[2:nrow(temp2)], temp2$lat[1]),
              border = ifelse(is.na(border), PalCols[temp$colfunc2], border),
              col =  PalCols[temp$colfunc2])
    }
  }

  linePeru <- r4fish::map_coastline
  lines(linePeru$lon, linePeru$lat, col = land.border)
  polygon(x = c(linePeru$lon[1], -50, -50, linePeru$lon[23513:2], linePeru$lon[1]),
          y = c(linePeru$lat[1], -24, 0, linePeru$lat[23513:2], linePeru$lat[1]),
          col = land.border)
  axis(xaxis, at = seq(-84, -70, by = 2), labels = paste0(seq(84, 70, by = -2), " °W"), cex.axis = 0.9)
  axis(xaxis, at = seq(-84, -70, by = 1), labels = NA)
  axis(yaxis, at = seq(-21, -3, by = 2), labels = paste0(seq(21, 3, by = -2), " °S"), las = 2, cex.axis = 0.9)
  axis(yaxis, at = seq(-21, -3, by = 1), labels = NA)
  box()

  if(!is.na(portImport)){
    map_harbor <- r4fish::map_harbordata
    nameSel <- paste0("shw.", portImport)
    map_harbor <- map_harbor[map_harbor[, nameSel] == 1, ]
    cexp = 0.5*portImport
    text(map_harbor$lon, map_harbor$lat, labels = map_harbor$name,
         cex = cexp, pos = 4)
  }

  if(legend){


    diffX = 0.03*diff(xxlim)
    diffY = 0.41*diff(yylim)
    posX = min(xxlim) + (0.099)*diff(xxlim)
    posY = min(yylim) + (0.03)*diff(yylim)

    if(typeval == "#"){
    legend.krige(x.leg = c(posX, posX+diffX), y.leg = c(posY, posY+diffY),
                 values = minCols:maxCols, vertical=T,
                 col = colfunc(totCols),
                 offset.leg = 0.75)
    }

    if(typeval == "%"){
      legend.krige(x.leg = c(posX, posX+diffX), y.leg = c(posY, posY+diffY),
                   values = 0:100, vertical=T,
                   col = colfunc(101)[-1],
                   offset.leg = 0.75)
    }


    text(x = (posX+diffX/2), y = (posY+diffY)*0.95, labels = paste0(txtleg),
         cex = 0.8, font = 2)
    }


  if(save){
    FileName = paste("MapPeruvianGrid", by, sep = "_")
    FileName = file.path(cout, paste0(FileName, ".png"))
    dev.copy(png, filename = FileName,  width = 1400, height = 1800, res = 300)
    while (dev.cur()>1) dev.off()
  }

  return(invisible())
}
