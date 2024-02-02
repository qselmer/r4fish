rm(list = ls())
#  -----------------------------------------------------------------------
require(devtools)
require(roxygen2)
session_info()

getwd()

check()
document()
build()

# install(reload = T, force = T)
# remotes::install_github("qselmer/r4fish")
# load("data/species2.RData")
# fix(species2)

##
roxygenise()
build_readme()
