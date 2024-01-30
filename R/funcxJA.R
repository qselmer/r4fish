# -------------------------------------------------------------------------
#' Title
#'
#' @return
#' @export
#'
#' @examples
script_wd <- function(){
  dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(dir)
  getwd()
}

# -------------------------------------------------------------------------
#' Title
#'
#' @param file
#'
#' @return
#' @export frequency_matrix
#'
#' @examples
read_freq_F1 <- function(file){
  require(readxl)
  data <- read_excel(path = file, sheet = 1, skip = 3)
  marks <- as.numeric(unlist(data[,1]))
  time <-  as.numeric(colnames(data)[-1])
  frec <- unname(as.matrix(data[,-1]))
  frec[is.na(frec)] <- 0
  colnames(frec) <- time
  rownames(frec) <- marks
  class(frec) <- "frequency_matrix"
  print(class(frec))
  return(frec)
}


# function n°1:-------------------------------------------------------------------------
#' Title
#'
#' @param x
#' @param relative
#' @param clean
#' @param ylim
#' @param yinter
#' @param juvMarks
#' @param plotCol
#' @param juvCol
#' @param textx
#' @param type
#'
#' @return
#' @export
#' @rdname plot
#' @examples
plot.frequency_matrix <- function(x,
                                  relative = T,
                                  clean = T,
                                  ylim = c(0, 20),
                                  yinter = 5,
                                  juvMarks = 51,
                                  plotCol = "blue",
                                  juvCol = "red",
                                  textx = "Longitud total (cm)",
                                  type = "barplot" #barplot
                                  ){
  require(r4fish)

  frec <- x
  if (isTRUE(relative)) {frec <- apply(frec, MARGIN = 2, FUN = normalize)}
  base <- frec
  ncols <- ncol(base)
  marks <- as.numeric(rownames(base))

  par(mfrow = c(ncols, 1), oma = c(5,7,2,7), mar = c(0,0,0,0))

  for (p in 1:ncols) {

    fig <- base[,p]

    if (type == "lines") {
      if (isTRUE(clean)) {fig <- cleanZeros(fig)}; fig <- as.vector(fig)
      plot(marks, fig, xlab = "", ylab = "", type = "l", lwd = 2,
           xaxt = "n", yaxt = "n", ylim = ylim, col = plotCol,
           xaxs = "i",yaxs = "i")
      abline(v = juvMarks, lty = 3, col = juvCol, lwd = 2)
    }

    if (type == "barplot") {

      density <- ifelse(marks < juvMarks, 40, 100)

      bp <- barplot(height = fig, space = 0,  xlab = "", ylab = "", xaxt = "n", yaxt = "n",
                    ylim = ylim, col = plotCol, border = "gray10", xaxs = "i",yaxs = "i",
                    density = density); box()
    }

    if (isTRUE(relative)) {
      juv <- sum(fig[marks < juvMarks], na.rm = T)
    }else{
      juv <- (sum(fig[marks < juvMarks], na.rm = T)/sum(fig, na.rm = T))*100
    }

    if (p %% 2 == 0) {aty = 4}else{aty = 2}
    axis(aty, at = seq(ylim[1],ylim[2], yinter), labels = seq(ylim[1],ylim[2], yinter),
         las = 2, cex.axis = 1.5)
    if (p == ncols) {
      if (type == "lines") {
        axis(1, marks, labels = NA, cex.axis = 1.2)
        axis(1, marks[seq(from = 1, by = 2, length.out = length(marks)/2)],
             cex.axis = 1.5)
      }
      if (type == "barplot") {
        axis(1, at = bp, labels =  marks,  cex.axis = 1.5)
      }

      mtext(text =  textx, side = 1, line = 3.5, outer = TRUE, cex = 1.5)
      mtext(text =  ifelse(isTRUE(relative), "Frecuencia relativa (%)", "Frecuencia"),
            side = 2, line = 4, outer = TRUE, cex = 1.5)
    }

    mtext(text = paste("juv =",round(juv,0), "%"),side = 3, adj = 0.02, line = -2, cex = 1.5)
    mtext(text = colnames(base)[p] ,side = 3, adj = 0.97, line = -2, cex = 1.5)
  }
}
