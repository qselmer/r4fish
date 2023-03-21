
#' Identifica errores y valida la base de datos
#'
#' @param data datos del crucero en formato MF
#' @param sp especie objetivo de la evaluación
#' @param stock stock objetivo de la evaluación
#' @param cout vector de caracteres que contienen la ruta de salida
#' @param file ector de caracteres que contienen el nombre del archivo
#'
#' @return
#' @export
#'
#' @examples#'
#' \code{validateSp(data = data, sp = "jurel", stock = NA, cout = "outputs",
#' file = "document.docx")}
#'
validateSp <- function(data = data,
                       sp = "anchoveta",
                       stock = NA,
                       cout = "outputs",
                       file = "validateSp.html"){
  require(pander)
  require(sp)
  require(r4fish)

# -------------------------------------------------------------------------


  templ <-system.file("rmarkdown", "templates",
                      "InformationVesselSet.Rmd",
                      package = "r4fish")

  rmarkdown :: render(input = templ,
                      output_dir = cout,
                      output_file = file)
  cat("Done")
  return(invisible())
}





