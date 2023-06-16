# -------------------------------------------------------------------------
## obtiene información del especie-stock
.getSp <- function(sp = NA, stock = NA){
  spx <- r4fish::species2
  spx <- spx[spx$tradename == sp & spx$stock == stock, ]
  spx <- spx[!is.na(spx$id),]
  return(spx)
  }
# -------------------------------------------------------------------------
## obtiene marcas de longitud por especie, y los bordes de marcas
.getMarks <- function(sp = NA, stock = NA, phi = FALSE){
  spx <- .getSp(sp = sp, stock = stock)
  all.marks <- seq(from = spx$lengthMin, to = spx$lengthMax,
                   by = spx$lengthBin)
  if(isTRUE(phi)) {
    marks_inf <- all.marks - 0.5*spx$lengthBin
    marks_sup <- all.marks + 0.5*spx$lengthBin
    all.marks <- sort(unique(c(marks_inf, marks_sup)))
  }
  return(all.marks)
}
# -------------------------------------------------------------------------
## obtiene marcas de edad por especie, y los bordes de edad
.getAges <- function(sp = NA, stock = NA, phi = FALSE){
  spx <- .getSp(sp = sp, stock = stock)
  all.ages <- seq(from = spx$ageMin, to = spx$ageMax,
                   by = spx$ageBin)
  if(isTRUE(phi)) {
    ages_inf <- all.ages - 0.5*spx$ageBin
    ages_sup <- all.ages + 0.5*spx$ageBin
    all.ages <- sort(unique(c(ages_inf, ages_sup)))
  }
  return(all.ages)
}
# -------------------------------------------------------------------------
## obtiene la latitud de la especie
.getLati <- function(sp = NA,
                    stock = NA) {
  spx <- .getSp(sp = sp, stock = stock)
  lat <- unname(unlist(spx[, c("latMin", "latMax")]))
  return(lat)
}
# -------------------------------------------------------------------------
## obtiene la talla juvenil de la especie
.getJuv <- function(sp = NA,
                   stock = NA) {
  spx <- .getSp(sp = sp, stock = stock)
  juv <- unlist(spx[, c("juvenile")])
  return(juv)
}
# -------------------------------------------------------------------------
## obtiene los parámetros globales de la especie: L-P
.getLP <- function(sp = NA,
                  stock = NA) {
  spx <- .getSp(sp = sp, stock = stock)
  pars <- unname(unlist(spx[, c("LWa", "LWb")]))
  pars <- list(a = pars[1], b = pars[2])
  return(pars)
}
# -------------------------------------------------------------------------
## obtiene peso de la especie. opcional da la función inversa
.length2weight <- function(sp = NA, stock = NA, ab = NA, funx.inv = F){
  marks <- .getMarks(sp = sp, stock = stock, phi = F)
  if(is.list(ab)){
    ab <- ab
    }else{
      if(is.na(ab)){ab <- .getLP(sp = sp, stock = stock)}
    }

  w.spx <- ab[[1]]*(marks^ab[[2]])

  if(funx.inv){
    require(rootSolve)
    f <- function(x) ab[[1]] * (x^ab[[2]])
    invf <- inverse(f = f, lower = 0, upper = 100)
    }

  if(funx.inv) {
    return(list(weight = w.spx, inv.funx = invf))
    }else{
      return(w.spx)
    }
}
# -------------------------------------------------------------------------
## obtiene los parámetros globales de la especie: crecimiento corporal
.getGrowth <- function(sp = NA, stock = NA) {
  spx <- .getSp(sp = sp, stock = stock)
  pars <- unname(unlist(spx[, c("growthLinf", "growthK", "growthT0")]))
  pars <- list(Linf = pars[1], k = pars[2], t0 = pars[3])
  return(pars)
}
# -------------------------------------------------------------------------
## obtiene la longitud de la especie a la edad x. opcional da la función inversa
.ages2length <- function(sp = NA, stock = NA, growth = NA, funx.inv = F){
  t <- .getAges(sp = sp, stock = stock)
  if(is.list(growth)){
    growth <- growth
  }else{
    if(is.na(growth)){growth <- .getGrowth(sp = sp, stock = stock)}
  }

  w.spx <- growth[[1]] * (1 - exp(-growth[[2]] * (t - growth[[3]])))

  if(funx.inv){
    require(rootSolve)
    f <- function(x) growth[[1]] * (1 - exp(-growth[[2]] * (x - growth[[3]])))
    invf <- inverse(f = f, lower = 0, upper = 100)
  }

  if(funx.inv) {
    return(list(lt = w.spx, inv.funx = invf))
  }else{
    return(w.spx)
  }
}
# -------------------------------------------------------------------------
## obtiene los parámetros globales de la especie: selectividad
.getSelect <- function(sp = NA, stock = NA, method = "log3") {
  spx <- .getSp(sp = sp, stock = stock)
  if(method == "log3"){pars <- unname(unlist(spx[, c("selectivity3Par1", "selectivity3Par2")]))}
  if(method == "log19"){pars <- unname(unlist(spx[, c("selectivity19Par1", "selectivity19Par2")]))}
  pars <- list(par1 = pars[1], par2 = pars[2])
  return(pars)
}
# -------------------------------------------------------------------------
## obtiene la probabilidad de la especie a la talla x. opcional da la función inversa
.length2selc <- function(sp = NA, stock = NA, selc = NA,  method = "log3",
                           funx.inv = F){
  marks <- .getMarks(sp = sp, stock = stock, phi = F)

  if(is.list(selc)){
    selc <- selc
  }else{
    if(is.na(selc)){selc <- .getSelect(sp = sp, stock = stock, method = method)}
  }

  if(method == "log3"){
    w.spx <- 1 /(1 + exp((log(3) / selc[[2]]) * (selc[[1]] - marks)))}
  if(method == "log19"){
    w.spx <- 1/(1 + exp(-log(19)*(marks-selc[[1]])/selc[[2]]))}

  if(funx.inv){
    require(rootSolve)
    if(method == "log3"){
      f <- function(x) 1 /(1 + exp((log(3) / selc[[2]]) * (selc[[1]] - x)))}
    if(method == "log19"){
      f <- function(x) 1/(1 + exp(-log(19)*(x-selc[[1]])/selc[[2]]))}
    invf <- inverse(f = f, lower = 0, upper = 100)
  }

  if(funx.inv) {
    return(list(prob = w.spx, inv.funx = invf))
  }else{
    return(w.spx)
  }
}
# -------------------------------------------------------------------------
## obtiene la probabilidad de la especie a la talla x. opcional da la función inversa









imageSp <- function(sp, stock){

  require(httr)
  require(png)
  spx <- r4fish:::.getSp(sp = sp, stock = stock)
  url <- spx$photo
  # Descargar la imagen desde la URL
  response <- GET(url)
  content <- content(response, as = "raw")
  # Guardar la imagen en un archivo temporal
  temp_file <- tempfile(fileext = ".png")
  writeBin(content, temp_file)
  imagen <- readPNG(temp_file)
  plot(NA, xlim = c(1, dim(imagen)[2]), ylim = c(1, dim(imagen)[1]), type = "n", xlab = "", ylab = "")
  rasterImage(imagen, 1, 1, dim(imagen)[2], dim(imagen)[1])
  return(invisible())
}
