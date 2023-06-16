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
## obtiene los parámetros globales de la especie: madurez
.getMad <- function(sp = NA, stock = NA) {
  spx <- .getSp(sp = sp, stock = stock)
  pars <- unname(unlist(spx[, c("maturityMat1", "maturityMat2")]))
  pars <- list(par1 = pars[1], par2 = pars[2])
  return(pars)
}
# -------------------------------------------------------------------------
## obtiene la probabilidad de la especie a la talla x. opcional da la función inversa
.length2mad <- function(sp = NA, stock = NA, mad = NA, funx.inv = F){
  marks <- .getMarks(sp = sp, stock = stock, phi = F)
  if(is.list(mad)){
    mad <- mad
  }else{
    if(is.na(mad)){mad <- .getMad(sp = sp, stock = stock)}
  }

  w.spx <- 1/(1 + exp((mad[[1]] - (marks*mad[[2]]))))

  if(funx.inv){
    require(rootSolve)
    f <- function(x) 1/(1 + exp((mad[[1]] - (x*mad[[2]]))))
    invf <- inverse(f = f, lower = 0, upper = 100)
  }

  if(funx.inv) {
    return(list(prob = w.spx, inv.funx = invf))
  }else{
    return(w.spx)
  }
}
# -------------------------------------------------------------------------
## obtiene tallas de pre-reclutas
.getPrerecruits <- function(sp = NA, stock = NA){
  spx <- .getSp(sp = sp, stock = stock)
  rcr <- unname(unlist(spx[, c("prereclutas")]))
  rcr <- asn(unlist(strsplit(x = rcr, split ="-")))
  rcr <- seq(rcr[1], rcr[2], by = spx[, "lengthBin"])
  return(rcr)
}
# -------------------------------------------------------------------------
## obtiene tallas de reclutas
.getRecruits <- function(sp = NA, stock = NA){
  spx <- .getSp(sp = sp, stock = stock)
  rcr <- unname(unlist(spx[, c("reclutas")]))
  rcr <- asn(unlist(strsplit(x = rcr, split ="-")))
  rcr <- seq(rcr[1], rcr[2], by = spx[, "lengthBin"])
  return(rcr)
}
# -------------------------------------------------------------------------
## obtiene tallas de adultos
.getAdults <- function(sp = NA, stock = NA){
  spx <- .getSp(sp = sp, stock = stock)
  rcr <- unname(unlist(spx[, c("adulto")]))
  rcr <- asn(unlist(strsplit(x = rcr, split ="-")))
  rcr <- seq(rcr[1], rcr[2], by = spx[, "lengthBin"])
  return(rcr)
}
# -------------------------------------------------------------------------
## obtiene informacion de medidas de HCR
.getTAC <- function(sp = NA, stock = NA){
  spx <- .getSp(sp = sp, stock = stock)
  spTAC <- unname(unlist(spx[, c("BDRlim", "Eref")]))
  spTAC <- list(BDR = spTAC[1], E = spTAC[2])
  return(spTAC)
}
# -------------------------------------------------------------------------
##
.imageSp <- function(sp, stock, x = 0, y = 0, cex = 2){
  require(png)
  path <- paste0(sp, ".png")
  templ <- system.file("fig", path, package = "r4fish")
  img <- readPNG(templ)
  asp <- dim(img)[2] / dim(img)[1]
  rasterImage(img, x, y, x+asp*cex, y+asp*cex)
  return(invisible())
}
# -------------------------------------------------------------------------


