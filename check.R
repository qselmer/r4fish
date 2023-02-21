rm(list = ls())
#  -----------------------------------------------------------------------
require(devtools)
getwd()
build()

# load(file = "data/species.RData")
# str(species)


install(reload = T, force = T)
document()

check()

