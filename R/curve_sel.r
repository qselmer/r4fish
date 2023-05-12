#' Title
#'
#' @param sp
#' @param stock
#' @param par
#' @param method
#' @param add.plot
#' @param add.inv
#'
#' @return
#' @export
#'
#' @examples
curve_sel <- function(sp = NA,
                      stock = NA,
                      par = c(NA, NA),
                      method = "log3",
                      add.plot = T,
                      add.inv = T){

  require(ggplot2)
  #---
  marks <- r4fish:::.getMarks(sp = sp, stock = stock)
  sp <- r4fish:::.getSp(sp = sp, stock = stock)

  if(all(is.na(par))){
    if(method == "log3"){par = c(sp$log3.par1, sp$log3.par2)}
    if(method == "log19"){par = c(sp$log19.par1, sp$log19.par2)}
  }

  par1 = par[1]; par2 = par[2]

  if(method == "log3"){
    sel = 1/(1+ exp((par1*log(3)/par2)-(marks*log(3)/par2)))
    inv = inverse(function (x) 1/(1+ exp((par1*log(3)/par2)-(x*log(3)/par2))),
                  0.1, 100)
    par3 <- round(c(par1, par2), 2)
    eq = expression(italic(S[li] == frac(1,1+e^((-log(3)*frac("l"-par3[1], par3[2]))))))
      }
  if(method == "log19"){
    sel = 1/(1+ exp(-log(19)*(marks-par1)/par2))
    inv = inverse(function (x) 1/(1+ exp(-log(19)*(x-par1)/par2)),
                  0.1, 100)
    par3 <- round(c(par1, par2), 2)
    eq = expression(italic(S[li] == frac(1,1+e^((-log(19)*frac("l"-par3[1], par3[2]))))))
    }

  if(add.plot == TRUE){

    vec <- unlist(c(inv(.25),inv(.5), inv(.75)))
    min2 <- min(marks)
    data_lines <- data.frame(x = c(vec,min2,min2,min2),
                             y = c(0,0,0,.25,.50,.75),
                             xend = c(vec, vec),
                             yend = c(.25,.50,.75, .25,.50,.75),
                             col = rep(c("L25", "L50", "L75"), 2,
                             ))

    ylim.prim <- range(sel)
    ylim.sec <- range(1/sel)
    b <- diff(ylim.prim)/diff(ylim.sec)
    a <- ylim.prim[1] - b*ylim.sec[1]

    p <- ggplot(NULL, aes(x=marks, y=sel)) +
      geom_line(size = 1.2)

    if(add.inv == T){
      p <- p +
        geom_line(aes(x = marks, y = a + (1/sel)*b), size = 1,
                  color = "red", linetype = "dashed")
    }

    p <- p +
      scale_x_continuous(expand = c(0, 0),
                         breaks = range(marks)[1]: range(marks)[2],
                         limits = range(marks)) +
      scale_y_continuous(expand = c(0, 0),
                         sec.axis = sec_axis(~ (. - a)/b/1e4,
                                             name = "Inverse probability")
                         )



    p <- p +
      geom_segment(data = data_lines, aes(x = x, y = y,
                                          xend = xend, yend = yend,
                                          col = col),
                   size = 1)
    p <- p +geom_point(aes(vec, c(.25,.50,.75)), size = 3)


    p <- p +  theme(plot.title = element_text(face = "bold")) +
      theme_bw()
    p <- p +
      labs(
        x = "Total length (cm)",
        y = "Probability of catch",
        colour = "Selectivity",
        title =  toupper (rownames(sp))
      )

    p <- p + annotate("text", x = 16, y = 0.25,
                      label = eq)
    print(p)
  }

  out <- list(parSel = c(par1, par2), sel = sel)
  return(out)
}


