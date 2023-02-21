#' Title
#'
#' @param dat
#' @param col.var
#' @param col.factor
#' @param factor
#' @param cols.factor
#' @param box
#' @param model
#' @param lambda
#' @param IC.model
#' @param info
#' @param marf
#' @param omaf
#' @param hline
#' @param cline
#' @param bgf
#' @param lwdf
#' @param pchf
#' @param unitf
#' @param saveplot
#' @param outf
#' @param widthf
#' @param heightf
#' @param resf
#' @param labels.f
#' @param labels.y
#'
#' @return
#' @export
#'
#' @examples
g2plotSurvey <- function(dat = dat ,
                         col.var = "biomasa",
                         col.factor =  2, # "", number or string
                         factor = NA, #summ #winter

                         cols.factor = c("red","blue"),
                         box = "boxplot", #vioplot

                         model = "lm", #lm #smooth.spline (sp) #NA
                         lambda = 1e-4,
                         IC.model = TRUE,
                         info = TRUE,

                         marf = c(0.5,0.5,0,0.5),
                         omaf = c(3,4.5,2,2),

                         hline = TRUE,
                         cline = "white", #gray90
                         bgf = "white",
                         lwdf = 1,
                         ltyf =1,
                         pchf = 16,
                         cexf = 1.5,

                         unitf = 1e0,

                         saveplot = T,
                         outf = "./",
                         widthf = 1800,
                         heightf = 1200,
                         resf = 250,
                         labels.f = c("\nverano", "\ninvierno-\nprimavera"),
                         labels.y = "Biomasa ton", ...
                         ){

  require(vioplot)
  dat[, col.var] <- dat[, col.var]*unitf

  ylim <- range(dat[, col.var], na.rm = T)
  yy <- floor(log10(abs(ylim[2])))+1
  if(yy>2){ylim <- c(0, max(dat[, col.var], na.rm = T))}

  xchar <- 10^(yy-2)
  rrx <- ylim/xchar
  rr <- c(floor(rrx[1]), ceiling(rrx[2]))

  ylimb <- c(rr[1], rr[2], 1)
  ylimp <-  ylimb*xchar

  ##
  par(mar = marf, oma = omaf)
  nx <- 1
  nx <-  c(rep(rep(seq(1, (2*nx-1), 2), each = 2),3), rep(seq(2,2*nx, 2), each = 2))
  layout(mat = matrix(data = nx, ncol = 4, byrow = F))

  plot(dat$date, dat[, col.var], type = "n", yaxt  = "n", xaxt  = "n",
       xlab = "", ylab = "", ylim = c(ylimp[1], ylimp[2]),
       xaxs="i", xlim = range(addSeq(dat$date, 2)))

  year.lim <- c(floor(range(dat$date)[1]), ceiling(range(dat$date)[2]))
  dat$num.col <- as.numeric(as.factor(dat[, col.factor]))

  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = bgf)

  axis(side = 1, seq(year.lim[1], year.lim[2], .5) , tck= -0.01, labels= FALSE)
  axis(side = 1, year.lim[1]:year.lim[2], tck= -0.02, labels= year.lim[1]:year.lim[2], las = 2)

  axis(2, at = seq(ylimp[1], ylimp[2], ylimp[3]/2), labels= FALSE,
       las = 2, tck= -0.01)
  axis(2, at = seq(ylimp[1], ylimp[2], ylimp[3]), labels =  seq(ylimb[1], ylimb[2], ylimb[3]),
       las = 2, tck= -0.02)

  if(hline == T) abline(v = year.lim[1]:year.lim[2], lty = 1, lwd =2, col = cline)

  squared  <- bquote(".10"^.(log10(xchar)))
  if(xchar > 1){
    labels.x <- eval(bquote(expression(.(labels.y)*.(squared))))
  }else{
    labels.x <- labels.y
  }
  mtext(2, text = labels.x, line = 3)

  if(!is.na(factor)){
    tmp <-  dat[which(dat[, col.factor] == factor), ]
  }else{
    tmp <- dat}

  if(!is.na(model) & model == "lm"){
    lm_lt_max <- lm(formula = tmp[,col.var] ~ date, data = tmp)
    lm_newdata <- data.frame(date = addSeq(sec = tmp$date, add = 8))
    lm_yy <- predict.lm(object = lm_lt_max,  newdata = lm_newdata,
                        interval = 'confidence', level = .99)
    }

  if(!is.na(model) & model == "sp"){
    sp_lt_max  <- smooth.spline(tmp$date, tmp[,col.var], cv = T, lambda = lambda)
    sp_newdata <- addSeq(sec = tmp$date, add = 8)
    sp_yy <- predict(sp_lt_max, sp_newdata, interval = 'prediction')
    sp_res <- sp_lt_max$yin - sp_lt_max$y/(1-sp_lt_max$lev)
    sp_sigma <- sqrt(var(sp_res))
    sp_upper <- sp_lt_max$y + 2.0*sp_sigma*sqrt(sp_lt_max$lev)   # upper 95% conf. band
    sp_lower <- sp_lt_max$y - 2.0*sp_sigma*sqrt(sp_lt_max$lev)   # lower 95% conf. band
    }


  modl = NULL

  if(!is.na(model) & model == "lm"){

    if(IC.model){polygon(c(rev(lm_newdata$date), lm_newdata$date),
                         c(rev(lm_yy[ ,3]), lm_yy[ ,2]), col = 'grey70', border = NA)}
    lines(lm_newdata$date, lm_yy[,1], lwd = 2, col = 1)
  }

  if(!is.na(model) & model == "sp"){
    if(IC.model){
      polygon(c(rev(sp_lt_max$x), sp_lt_max$x), c(rev(sp_upper), sp_lower),
              col = 'grey70', border = NA)}
    lines(sp_yy$x, sp_yy$y,lwd = 2, col= 1)
  }


  t.nr <- shapiro.test(x = tmp[, col.var])

  if(t.nr$p.value > 0.05){
    mx <- mean(tmp[, col.var], na.rm = T)
    textx <- bquote(bar(x)*":" ~ .(round(mx, 1))*" (mean)")
  }else{
    mx <- median(tmp[, col.var], na.rm = T)
    textx <- bquote(tilde(x)*":" ~ .(round(mx, 1))*" (median)")
  }

  if(info){
    mtext(1, text = paste0("n: ", nrow(tmp)), line = -4.6, adj = 0.98, cex = 0.7)
    mtext(1, text = textx, line = -3.6, adj = 0.98, cex = 0.7)
    mtext(1, text = ifelse(t.nr$p.value > 0.05, "Normal distribution", "No-normal distribution"),
          line = -2.6, adj = 0.98, cex = 0.7)
    mtext(1, text = paste0(tmp$labels[1], " : ", tmp[nrow(tmp), ]$labels),
          line = -1.6, adj = .98, cex = 0.7)
  }

  abline(h = round(mx, 1), lty = 2)

  if(is.na(model)){
    lines(tmp$date, tmp[, col.var], col = 1, lwd  = lwdf, cex = 1.5, lty = ltyf)
  }

  if(is.na(cexf)){
    OldMin = min(tmp[, col.var+1]) ; OldMax =  max(tmp[, col.var+1])
    NewMin  = 2; NewMax = 12
    OldRange = (OldMax - OldMin)
    NewRange = (NewMax - NewMin)
    NewValue = (((tmp[, col.var+1] - OldMin) * NewRange) / OldRange) + NewMin
    cexf <- NewValue
    }

  points(tmp$date, tmp[, col.var], col = cols.factor[tmp$num.col], pch  = pchf, cex = cexf)
  box()

  if(box == "boxplot"){

    plt <-boxplot(tmp[, col.var] ~ tmp[, col.factor],yaxt  = "n", xaxt  = "n",
                  xlab = "", ylab = "",  ylim = c(ylimp[1], ylimp[2]))
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = bgf)
    par(new = T)
    boxplot(tmp[, col.var] ~ tmp[, col.factor], ylim = c(ylimp[1], ylimp[2]),
            col = NULL, border = cols.factor[tmp$num.col],
            yaxt  = "n", xaxt  = "n",  xlab = "", ylab = "")
    pltl <- length(unique(tmp[, col.factor]))
    text(1:pltl, rep(par("usr")[3]+ylimp[3], pltl), labels = paste("n: ", plt$n))
  }

  if(box == "vioplot"){

    plt<- vioplot(tmp[, col.var] ~ tmp[, col.factor],yaxt  = "n", xaxt  = "n",
                  xlab = "", ylab = "",   ylim =  c(ylimp[1], ylimp[2]))
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = bgf)
    par(new = T)
    vioplot(tmp[, col.var] ~ tmp[, col.factor], col = 0,
            border = cols.factor[tmp$num.col],
            yaxt  = "n", xaxt  = "n",   ylim = c(ylimp[1], ylimp[2]),
            xlab = "", ylab = "")

  }


  df.last <- tmp[nrow(tmp),]
  if(!is.na(factor)){
    points(1, df.last[, col.var],  cex = 2, col = cols.factor[df.last$num.col], pch = 16)
    points(1, df.last[, col.var], cex = 2, col = 1, pch = 1)

  }else{
    points(df.last$num.col, df.last[, col.var], cex = 2, col = cols.factor[df.last$num.col], pch = 16)
    points(df.last$num.col, df.last[, col.var], cex = 2, col = 1, pch = 1)

  }


  axis(side = 1, at = 1:2,  labels = F, tick = 0)
  axis(side = 1, at = 1:2,  labels = labels.f, tick = F, cex.axis= 1)

  fileName = paste(substring(labels.y, 1,6),
                   ifelse(is.na(factor), "allseason", factor),
                   box,
                   ifelse(is.na(model), "Nofit", model),
                   paste0("From", year.lim[1], "to", year.lim[2]), sep = "-")

  fileName = paste0(outf, fileName, ".png")

  if(saveplot){
    dev.copy(png, filename = fileName,  width = widthf, height = heightf, res = resf)
    dev.off()
  }

  out <- list(dat = tmp, model = modl, test = t.nr)


  return(out)

  }
