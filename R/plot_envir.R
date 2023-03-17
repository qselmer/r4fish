# envir_scenario <- read.csv("J:/.database/auxdata/envir_scenario.csv", encoding = "latin1")
#
# envir_scenario <- list(envir_scenario, "http://met.igp.gob.pe/elnino/lista_eventos.html")
#
# save(envir_scenario, file = "data/envir_scenario.RData")

load("data/envir_scenario.Rdata")

#' Title
#'
#' @param what
#' @param year.limit
#' @param ylim
#' @param magnitude
#' @param axis.x
#'
#' @return
#' @export
#'
#' @examples
plot_envir <- function(what = NA,
                       year.limit = c(1990,2022),
                       ylim = c(-2,3.5),
                       magnitude = F,
                       axis.x = T
                       ){

  require(stringi)
  envir <- envir_scenario[[1]]
  if(is.na(what)){
    envir <- envir
    colx <- c("red", "blue")
  }else{
    envir <- envir[envir$what == what, ]
    colx <- ifelse(what == "niño", "red", "blue")
  }

  envir <- envir[envir$year_start >= year.limit[1] &
                   envir$year_start <= year.limit[2], ]

  sc <- rev(sort(unique(envir$what)))
  n_sc <- length(sc)

  plot(1,1, xlim = year.limit, ylim = ylim, xlab = "", ylab = "",
       xaxt = "n", yaxt = "n")

  axis(1, at = seq(year.limit[1], year.limit[2], 1/4), labels = F, tcl = -0.2)
  axis(1, at = seq(year.limit[1], year.limit[2], 1), labels = axis.x)

  for(e in 1:n_sc){
    tmp <- envir[envir$what == sc[e], ]
    tmp$init <- tmp$year_start + tmp$month_start/12 -1/12
    tmp$end <- tmp$year_end + tmp$month_end/12
    tmp$magnitude <- tolower(stri_trans_general(tmp$magnitude, "Latin-ASCII"))
    tmp$magnitude <- factor(tmp$magnitude, levels = c("debil", "moderado",
                                                      "fuerte", "extraordinario"))

    if(magnitude == T){tmp$col.mag <- as.integer(tmp$magnitude)
    }else{
      tmp$col.mag <- 1
    }

    for(i in 1:nrow(tmp)){

      tmp_i <- tmp[i, ]
      ry <- diff(ylim)
      polygon(x = c(tmp_i$init, tmp_i$end, tmp_i$end, tmp_i$init),
              y = c(ylim[1]-ry, ylim[1]-ry, ylim[2]+ry, ylim[2]+ry), border = NA,
              col = adjustcolor(colx[e], alpha.f = tmp_i$col.mag*0.25 ))

    }


  }

  return("http://met.igp.gob.pe/elnino/lista_eventos.html")

}
