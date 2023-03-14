#' Title
#'
#' @param f
#' @param lower
#' @param upper
#'
#' @return
#' @export
#'
#' @examples
inverse <- function (f, lower = -100, upper = 100) {

  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]

  }

#' Title
#'
#' @param sp
#' @param stock
#'
#' @return
#' @export
#'
#' @examples
get.sp <- function(sp = NA, stock = NA){

  sp0 <- species
  sp0 <- sp0[sp0$tradename == sp & sp0$stock == stock, ]
  return(sp0)
}

#' Title
#'
#' @param sp
#' @param stock
#' @param phi
#'
#' @return
#' @export
#'
#' @examples
make.marks <- function(sp = NA, stock = NA, phi = FALSE) {

  sp <- get.sp(sp = sp, stock = stock)
  all.marks <- seq(from = sp$lmin, to = sp$lmax, by = sp$bin.l)

  if(isTRUE(phi)) {
    marks_inf <- all.marks - 0.5*sp$bin.l
    marks_sup <- all.marks + 0.5*sp$bin.l
    all.marks <- sort(unique(c(marks_inf, marks_sup)))
  }
  return(all.marks)
}


#' Title
#'
#' @param sp
#' @param stock
#'
#' @return
#' @export
#'
#' @examples
range_lat <- function(sp = NA, stock = NA) {
  sp <- get.sp(sp = sp, stock = stock)
  lat <- unlist(sp[, c("lat.min", "lat.max")])
  return(lat)
}

