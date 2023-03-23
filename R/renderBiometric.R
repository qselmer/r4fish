
#' Crea, unifica archivo biometrico
#'
#' @param cin vector de caracteres que contienen la ruta de entrada
#' @param cout vector de caracteres que contienen la ruta de salida
#' @param file vector de caracteres que contienen el nombre del archivo
#' @param save logico; ¿Se deben guardar el archivo?
#'
#' @return
#' @export
#'
#' @examples
#' \code{renderBiometric(cin =  "input",
#' cout = NA,
#' file = "Biometria por especies 03032023.xlsx",
#' save = T)}
#'
renderBiometric <- function(cin =  "inputs",
                            cout = ".",
                            file = "data.xlsx",
                            encoding = "latin",
                            save = T){
  require(rio)
  require(tools)
  require(TBE)
  # -------------------------------------------------------------------------
  file <- file.path(path = cin, file)
  if(is.na(cout)){cout = cin}
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
      name <- read.csv(file = file, sep = sep,
                       stringsAsFactors = FALSE, encoding = encoding)
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

    Encoding(base$sp) <- encoding
    base$sp <- iconv(x = base$sp, to="ASCII//TRANSLIT")

    Encoding(base$crucero) <- encoding
    base$crucero <- iconv(x = base$crucero, to="ASCII//TRANSLIT")

    base$sp <- tolower(base$sp)
    base$buque <- as.character(base$buque)
    base$fecha <- as.character(base$fecha)
    base$lon <- -abs(base$lon)
    base$lat <- -abs(base$lat)
  }

  unlink(file)
  print(name)
  if(save == T){
    file.name <- paste0(name, ".", pattern)
    file.name <- file.path(path = cout, file.name)
    write.csv(base, file = file.name, row.names = F)
  }


  return(base)
}
