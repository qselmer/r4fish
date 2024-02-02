rm(list = ls())
#  -----------------------------------------------------------------------
require(devtools)
require(roxygen2)
session_info()

getwd()
build()
document()
# install(reload = T, force = T)
# remotes::install_github("qselmer/r4fish")
# load("data/species2.RData")
# fix(species2)

##
roxygenise()
build_readme()
build_manual()
check()
