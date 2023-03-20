rm(list = ls())
#  -----------------------------------------------------------------------
require(devtools)
session_info()

getwd()

check()
build()
install(reload = T, force = T)
document()


require(r4fish)
plot_envir(what = NA, year.limit = c(1990, 2023), ylim = c(-4,5),
           magnitude = T, axis.x = T)
