weighting <- function(marks, freq, ab, weight,
                      uN = 1e6, uW = 1e6){
  weight <- weight*uW #peso en gramos
  freq <- freq*uN #numero de individuos
  peso <- ab[1]*(marks^ab[2])
  fx <- weight/sum(peso*freq)
  new.freq <- freq*fx
  out <- list(factor = fx, new.freq = new.freq)
  return(out)
}
