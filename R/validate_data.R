
validate_data <- function(data = data,
                          sp = "anchoveta",
                          output_dir = "outputs",
                          output_file = "Information.docx"){

  rmarkdown :: render(input = "InformationVesselSet.Rmd",
                      output_dir = output_dir,
                      output_file = output_file)
  return(invisible())
}





