
validateSp <- function(data = data,
                       sp = "anchoveta",
                       cout = "outputs",
                       file = "validateSp.docx"){

  templ <-system.file("rmarkdown", "templates",
                      "InformationVesselSet.Rmd",
                      package = "r4fish")

  rmarkdown :: render(input = templ,
                      output_dir = cout,
                      output_file = file)
  return("Done")
}





