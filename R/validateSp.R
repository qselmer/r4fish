
#' Identifica errores y valida la base de datos
#'
#' @param data
#' @param sp
#' @param stock
#' @param cout
#' @param file
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

  templ <-system.file("rmarkdown", "templates",
                      "InformationVesselSet.Rmd",
                      package = "r4fish")

  rmarkdown :: render(input = templ,
                      output_dir = cout,
                      output_file = file)
  return("Done")
}





