
validate_data <- function(data = data,
                          sp = "anchoveta",
                          output_dir = "outputs",
                          output_file = "Information.docx"){

  templ <- system.file("templates",
                       "InformationVesselSet.Rmd",
                       package = "r4fish")

  rmarkdown :: render(input = templ,
                      output_dir = output_dir,
                      output_file = output_file)
  return(invisible())
}





