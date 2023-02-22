rm(list = ls())
#  -----------------------------------------------------------------------
require(devtools)
session_info()

getwd()
build()
install(reload = T, force = T)
document()
check()


