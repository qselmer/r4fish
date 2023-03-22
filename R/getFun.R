.getSp <- function(sp = NA, stock = NA){

  sp0 <- r4fish::species

  if(is.na(stock)){
    sp.x <- sp0[sp0$tradename == sp,]
    }else{
      sp.x <- sp0[sp0$tradename == sp & sp0$stock == stock, ]
      }
  return(sp.x)
  }

# -------------------------------------------------------------------------
.getMarks <- function(sp = NA,
                     stock = NA,
                     phi = FALSE){
  sp <- .getSp(sp = sp, stock = stock)
  all.marks <- seq(from = sp$lmin, to = sp$lmax, by = sp$bin.l)

  if(isTRUE(phi)) {
    marks_inf <- all.marks - 0.5*sp$bin.l
    marks_sup <- all.marks + 0.5*sp$bin.l
    all.marks <- sort(unique(c(marks_inf, marks_sup)))
  }
  return(all.marks)
}

# -------------------------------------------------------------------------
.getLati <- function(sp = NA,
                    stock = NA) {
  sp <- .getSp(sp = sp, stock = stock)
  lat <- unlist(sp[, c("lat.min", "lat.max")])
  return(lat)
}


# -------------------------------------------------------------------------
.getJuv <- function(sp = NA,
                   stock = NA) {
  sp <- .getSp(sp = sp, stock = stock)
  juv <- unlist(sp[, c("juvenile")])
  return(juv)
}

# -------------------------------------------------------------------------
.getLP <- function(sp = NA,
                  stock = NA) {
  sp <- .getSp(sp = sp, stock = stock)
  ab <- unlist(sp[, c("a", "b")])
  ab <- list(a = ab[1], b = ab[2])
  return(ab)
}

