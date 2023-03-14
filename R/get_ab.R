# -------------------------------------------------------------------------
get_ab <- function( file = "xxx.csv",
                    dir = "inputs",
                    sp =   "anchoveta",
                    stock = "nc",
                    plot.ab = T){

  require(TBE)

  dir_file <- file.path(dir, file)
  tmp <- read.csv(dir_file, header = T, encoding = "latin1");
  tmp <- write.csv(tmp, dir_file, row.names = F)

  data <- TBE:::getData_biometric(file = dir_file)
  name <- trimws(data[1, "crucero"])
  xlat <- range_lat(sp = sp, stock = stock)
  xmarks <- make.marks(sp = sp, stock = stock)
  data <- data[data$lat > xlat[1] & data$lat <= xlat[2],]

  check <- c("weight", "length") %in% names(data)
  if(!all(check)){
    stop(sprintf("Check variables in base %s", dataName))
  }

  index <- sapply(c("gonad", "evisc"), grepl, x = colnames(data))
  index <- all(apply(index, 2, any))

  if(!isTRUE(index)){
    data <- data[data$sp == sp, ]
  }

  if(!all(is.na(data$weight))){

  data.glm <- data[data$weight > 1 | !is.nan(data$weight), ]
  model <- glm(log(weight) ~ log(length), data = data.glm, na.action = na.omit)
  pars <- summary(model)$coefficients[, 1:2]
  b <- pars["log(length)", ]
  a <- pars["(Intercept)", ]
  ab <- list(a = as.numeric(exp(a[1])), b = as.numeric(b[1]))
  }else{
    ab <- list(a = NA, b = NA)
  }


  if(plot.ab == T){

    hb = 60
    tmp <- data[!is.na(data$weight), ]
    tmp <- tmp[tmp$weight > 1 | !is.nan(tmp$weight), ]
    tmp.t <- as.data.frame.table(table(tmp$length))
    tmp.t$Var1 <- as.numeric(as.character(tmp.t$Var1))
    tmp.t$Freq2 <- tmp.t$Freq/4

    plot(data$length, data$weight, pch = 16, xaxt = "n",
         yaxt = "n", xlim = range(xmarks), ylim = c(0, hb+20),
         xlab = "", ylab = "")
    axis(1, at = xmarks, labels = xmarks)
    axis(2, at = seq(0, hb, 5), labels = seq(0, hb, 5), las = 2)
    lines(xmarks, ab$a*(xmarks^ab$b), type = "l", lwd = 2,
          col = "brown2")

    abline(h = hb)

    text(x = xmarks[4], y =  hb-5,  labels = name, adj = 0)
    text(x = xmarks[4], y =  hb-15,
         labels = paste0("a =", round(ab$a, 2), ";",
                         "b =", round(ab$b, 2)), adj = 0)
    text(x = xmarks[4], y =  hb-25,
         labels = paste(length(data$length), length(tmp$length)),
         adj = 0)


    segments(x0 = tmp.t$Var1, y0 = rep(60, nrow(tmp.t)),
             x1 = tmp.t$Var1, y1 = tmp.t$Freq2+60, lwd  = 2,
             col = "gray10")
    axis(4, at = seq(60,80,5), labels =  seq(0,20,5)*4, las = 2)

  }

  return(ab)

}
