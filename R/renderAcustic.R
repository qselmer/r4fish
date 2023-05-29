renderAcustic <- (cin = "inputs/acustics/", cout = ".", file = "data.xlsx", 
                  encoding = "latin", save = T)  {
  
  require(TBE)
  require(tools)
  
  pattern <- file_ext(file.path(cin, file))
  
  if (is.null(file)) {
    return(NULL)
  }
  acousticData <- read.csv(file = file.path(cin, file),
                           stringsAsFactors = FALSE, encoding = encoding)
  colnames(acousticData) <- tolower(colnames(acousticData))
  
  if (!is.element("anc", colnames(acousticData)) || sum(an(acousticData$anc), 
                                                        na.rm = TRUE) < 1) {
    if (is.element("ancg", colnames(acousticData))) {
      if (is.element("ancp", colnames(acousticData))) {
        acousticData$anc <- an(apply(acousticData[, 
                                                  c("ancg", "ancp")], 1, sum, na.rm = TRUE))
      }
      else {
        acousticData$anc <- an(acousticData$ancg)
      }
    }
    else {
      stop("Acoustic base must have 'anc', 'ancg-ancp' or 'ancg' variables.")
    }
  }
  
  if (nchar(acousticData$date_m[1]) == 8){
    acousticDateFormat <- "%Y%m%d"
    }else{
      if (grepl(pattern = "/", x = acousticData$date_m[1], 
                 perl = TRUE)) {
        acousticDateFormat <- "%d/%m/%Y"
        }else{
          if (grepl(pattern = "-", x = acousticData$date_m[1], 
                 perl = TRUE)) {
            acousticDateFormat <- "%Y-%m-%d"
          }
        }
    }
  
  
if (is.element("time_m", colnames(acousticData))) {
    acousticData$date <- paste(gsub(x = acousticData$date_m, 
                                    pattern = " ", replacement = ""), gsub(x = acousticData$time_m, 
                                                                           pattern = " ", replacement = ""))
    acousticData$date <- strptime(acousticData$date, format = paste(acousticDateFormat, 
                                                                    "%H:%M:%S"))
  }else{
    acousticData$date <- as.POSIXct(acousticData$date_m, 
                                    format = acousticDateFormat)
    }
  
  acousticData$lon <- -abs(acousticData$lon_m)
  acousticData$lat <- -abs(acousticData$lat_m)
  acousticData$code <- TBE:::AIPcodeAssigner(dataPoints = acousticData[,c("lon", "lat")])
  
  index <- apply(acousticData[, c("lon", "lat", "date")], 
                 2, function(x) !is.na(x))
  index <- apply(index, 1, all)
  acousticData <- acousticData[index, ]
  
  if(is.element("cruise", colnames(acousticData))){
    name <- unique(acousticData[, c("cruise")])
  }else{
      name <- "renderAcustic"
    }
  
  print(name)
  if (save == T) {
    file.name <- paste0(name, ".", pattern)
    file.name <- file.path(path = cout, file.name)
    write.csv(acousticData, file = file.name, row.names = F)
  }
  
  return(acousticData)
}


  