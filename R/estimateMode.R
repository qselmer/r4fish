#' Estima moda de una frecuencia de tallas
#'
#' @param len vector completo de tallas
#' @param freq vector completo de frecuencias
#' @param tol.n valor de tolerancia del numero de la muestra
#' @param nmodes numero de modas a estimar
#' @param tol.freq valor de tolerancia para ser considerada moda
#'
#' @return
#' @export
#'
#' @examples
#'
estimateMode <- function(len,
                         freq,
                         tol.n = 150,
                         nmodes = 2,
                         tol.freq = 10){

  if(sum(freq, na.rm = TRUE) < tol.n){
    out = NA
    out3 = NA
  } else {

    if(length(freq[freq > 0]) == 1) {
      out = len[freq > 0]
      out3 = freq[freq > 0]
    } else {

      outDat = data.frame(l = len, frec =  freq)

      out = NULL
      out2 = NULL
      count = 0
      for(j in 2:(nrow(outDat) - 1)){

        val1 = outDat$frec[j-1]
        valmid = outDat$frec[j]
        val2 = outDat$frec[j+1]

        if(valmid > val1 & valmid == val2) {
          count = count + 1
          previo = val1
        }

        if(count == 1 & valmid < val2){
          count = 0
        }

        if(count == 1 & valmid == val1 & valmid > val2){
          poste = val2
          if(poste >= previo){
            mode = outDat$l[j]
          } else {
            mode = outDat$l[j-1]
          }
          out = c(out, mode)
          out2 = c(out2, valmid)
        }

        if(valmid > val1 & valmid > val2) {
          mode = outDat$l[j]
          out = c(out, mode)
          out2 = c(out2, valmid)
        }

        if(val1 > valmid & val1 > val2 & j == 2) {
          mode = outDat$l[j-1]
          out = c(out, mode)
          out2 = c(out2, val1)
        }

        if(val2 > valmid & val2 > val1 & j == (nrow(outDat) - 1)) {
          mode = outDat$l[j+1]
          out = c(out, mode)
          out2 = c(out2, val2)
        }

      }
      out3 = sort(out2, decreasing = T)
      out = out[order(out2, decreasing = T)]
      delPos = which(out3 < tol.freq)

      out[delPos] = NA
      out3[delPos] = NA
    }
  }

  out = out[1:nmodes]
  out3 = out3[1:nmodes]

  return(list(modas = out, frecuencia = out3))
}
