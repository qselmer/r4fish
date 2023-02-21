
#' Title
#'
#' @param dat
#' @param col.var
#' @param year.lim
#' @param col.set
#' @param set
#' @param col.factor
#' @param col.label
#' @param col.date
#' @param col.month
#' @param col.sems
#' @param col.year
#'
#' @return
#' @export
#'
#' @examples
subsetSurvey <- function(dat = dat, #data
                          col.var = c("biomasa","abundancia"), #number, string, seq
                          year.lim = c(1996, 2022), #limit year
                          col.set = "filter", #number or string
                          set = "ok", # filter string
                          col.factor = "season",
                          col.label = 2,
                          col.date = NA ,# "", number or string
                          col.month =  NA, # "", number or string
                          col.sems = "semester", # "", number or string
                          col.year = "year"# "", number or string)
                          ){

  names(dat) <- tolower(names(dat))
  if(!is.na(col.set)){ dat <- dat[grep(set, dat[, col.set]),] }
  if(!is.na(year.lim[1])){
    id.year <- which(dat[,col.year] >= year.lim[1] & dat[,col.year] <= year.lim[2])
    dat <- dat[id.year, ]
  }

  if(is.na(col.date)){
    if(is.na(col.month)){
      date.tmp <- dat[, col.year] + (2*(dat[, col.sems])-1)/4
      dat$date <- date.tmp
    }else{
      date.tmp <- dat[, col.year] + (dat[, col.month]/12)
      dat$date <- date.tmp
    }
  }else{
    date.tmp <- dat[, col.date]
    dat$date <- date.tmp
  }

  out <- cbind.data.frame(date = dat$date, factor = dat[, col.factor],
                          labels = dat[, col.label] , dat[ ,col.var])
  return(out)
}
