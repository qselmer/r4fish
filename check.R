rm(list = ls())
#  -----------------------------------------------------------------------
require(devtools)
session_info()

getwd()

check()
build()
install(reload = T, force = T)
document()


