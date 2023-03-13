#' Title
#'
#' @param dir
#' @param file
#' @param save
#'
#' @return
#' @export
#'
#' @examples
#'
get_biometric <- function(dir =  "rawdata",
                          file = "Biometria por especies 03032023.xlsx",
                          save = T){
  require(rio)
  require(tools)
  require(TBE)
  # -------------------------------------------------------------------------
  file <- file.path(path = dir, file)

  if (is.null(file)) {
    return(NULL)
  }

  pattern <- file_ext(file)

  if(pattern == "xlsx"){
    xls <- file
    created <- mapply(convert, xls, gsub("xlsx", "csv", xls))
    # unlink(xls) # delete xlsx files
    file <- as.vector(created)
    pattern <- file_ext(file)
  }

  if(pattern == "csv"){

    sep <- c(",", ";")
    sep <- sep[sapply(sep, function(x) grepl(x = readLines(con = file,
                                                           n = 1), pattern = x))]

    if (TBE:::isImarsis(file = file, sep = sep) | TBE:::isImarsis2(file = file, sep = sep)) {
      base <- TBE:::convertTBEbiomFormat(file, sep = sep)
      name <- read.csv(file)
      name <- toupper(name$NOMBRE_OPERACION[1])
      name <- iconv(x = name, to="ASCII//TRANSLIT")
    }else{
      if (TBE:::isMF(file = file, sep = sep)) {
        base <- TBE:::readMF(file, sep = sep)
      }else{
        base <- TBE:::readED(file, sep = sep)
      }
    }

    colnames(base) <- tolower(colnames(base))
    base$buque <- as.character(base$buque)
    base$fecha <- as.character(base$fecha)
    base$lon <- -abs(base$lon)
    base$lat <- -abs(base$lat)
  }

  if(save == T){
    file.name <- paste0(name, ".", pattern)
    file.name <- file.path(path = dir, file.name)
    write.csv(base, file = file.name, row.names = F)
  }

  return(base)

}
