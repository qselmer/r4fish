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
#' @param stk
#'
#' @return
#' @export
#'
#' @examples
get.sp <- function(sp = "anc", stock = NA){

  sp0 <- species
  sp0 <- sp0[sp0$tradename == sp & sp0$stock == stock, ]
  return(sp0)
}

#' Title
#'
#' @param sp
#' @param stk
#' @param phi
#'
#' @return
#' @export
#'
#' @examples
make.marks <- function(sp = "sp", stock = stock, phi = FALSE) {

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
#' @param stk
#' @param par
#' @param method
#' @param add.plot
#' @param add.inv
#'
#' @return
#' @export
#'
#' @examples
curve.sel <- function(sp = "anc",
                      stk = 1,
                      par = c(NA, NA),
                      method = "log3",
                      add.plot = T,
                      add.inv = T){

  require(ggplot2)
  #---
  marks <- make.marks(sp = sp, stk = stk)
  sp <- get.sp(sp = sp, stk = stk)

  if(all(is.na(par))){
    if(method == "log3"){par = c(sp$log3.par1, sp$log3.par2)}
    if(method == "log19"){par = c(sp$log19.par1, sp$log19.par2)}
  }

  par1 = par[1]; par2 = par[2]

  if(method == "log3"){
    sel = 1/(1+ exp((par1*log(3)/par2)-(marks*log(3)/par2)))
    inv = inverse(function (x) 1/(1+ exp((par1*log(3)/par2)-(x*log(3)/par2))),
                  0.1, 100)}
  if(method == "log19"){
    sel = 1/(1+ exp(-log(19)*(marks-par1)/par2))
    inv = inverse(function (x) 1/(1+ exp(-log(19)*(x-par1)/par2)),
                  0.1, 100)
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
                         limits = range(marks)) +
      scale_y_continuous(expand = c(0, 0) )


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
      y = "proportion",
      colour = "Selectivity",
      title =  toupper (rownames(sp))
      )
    print(p)
  }

  return(sel)


}


