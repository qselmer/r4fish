rm(list = ls())
#  -----------------------------------------------------------------------
require(devtools)
session_info()

getwd()

check()
document()
# build()
install(reload = T, force = T)

remotes::install_github("qselmer/r4fish")

load("data/KL2A_anchoveta.Rdata")
load("data/KL2A_anchoveta.Rdata")
