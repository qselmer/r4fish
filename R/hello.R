#' Plot biomasa y abundancia por crucero
#'
#' Grafica de biomasa y abundancia para el stock Norte-Centro de la anchoveta peruana
#' según los cruceros de investigación.
#' @param dat data.frame de la especie (Ver estructura)
#' @param sp nombre de la especie objetivo "anchoveta"
#' @param col.var index o nombres de la columnas que se utilizaran
#' @param year.lim  a character string indicating the name or names of the maturity stage.
#' @param box a character string indicating the method to be applied, \code{"fq"} frequentist GLM, or \code{"bayes"} Bayes GLM (MCMClogit function).
#' @param col.filter number of iterations (bootstrap resampling).
#' @param filter a single value, interpreted as an integer.
#' @param col.factor data.frame de la especie (Ver estructura)
#' @param factor nombre de la especie objetivo "anchoveta"
#' @param col.date index o nombres de la columnas que se utilizaran
#' @param col.month  a character string indicating the name or names of the maturity stage.
#' @param col.sems a character string indicating the method to be applied, \code{"fq"} frequentist GLM, or \code{"bayes"} Bayes GLM (MCMClogit function).
#' @param col.filter number of iterations (bootstrap resampling).
#' @param col.year a single value, interpreted as an integer.'
#' @return An object of class 'gonadMat'.
#' @param cols data.frame de la especie (Ver estructura)
#' @param plotAB nombre de la especie objetivo "anchoveta"
#' @param hline index o nombres de la columnas que se utilizaran
#' @param bgf  a character string indicating the name or names of the maturity stage.
#' @param lwdf a character string indicating the method to be applied, \code{"fq"} frequentist GLM, or \code{"bayes"} Bayes GLM (MCMClogit function).
#' @param unit number of iterations (bootstrap resampling).
#' @param outf a single value, interpreted as an integer.'
#' @param widthf a single value, interpreted as an integer.'
#' @param heightf a single value, interpreted as an integer.'
#' @param resf a single value, interpreted as an integer.'
#' @param marf a single value, interpreted as an integer.'
#' @param omaf a single value, interpreted as an integer.'
#' @param saveplot a single value, interpreted as an integer.'
#' @param labels.f a single value, interpreted as an integer.'
#' @return An object of class 'gonadMat'.
#' \code{model} the summary statistics of the model.
#'
#' \code{A_boot} the 'n iter' values of parameter A.
#'
#' \code{B_boot} the 'n iter' values of parameter B.
#'
#' \code{L50} the 'n iter' values of parameter L50 (size at gonad maturity).
#'
#' \code{out} a dataframe with the allometric variable "X", stage of sexual maturity, the fitted values for
#' logistic regression and confidence intervals (95\%). Also the summary statistics of the model is provided.
#' @details Estimate the size at gonad maturity using a logistic regression with X variable and
#' stages of sexual maturity (two categories: immature and mature).
#'
#' The function requires a data.frame with the X (allometric variable) and
#' the stage of sexual maturity (gonad maturation stage).
#'
#' The argument \code{varNames} requires a character string indicating the name of one allometric and the stage
#' of sexual maturity variable to be used for analysis (i.e. \code{varNames = c("total_length", "stage_mat")}).
#' So the argument \code{varNames} must contain two character strings only, the first is the allometric variable
#' and the second is the stage of sexual maturity.
#'
#' The arguments \code{inmName} and \code{matName} require a character string indicating the name
#' of the stages of sexual maturity in the data.frame. The argument could contain one character string
#' or should be a vector (i.e. \code{inmName = "I"}, \code{matName = c("II", "III", "IV")}).
#'
#' The argument \code{method} requires a character string indicating which regression will be used for the test.
#' If \code{method = "fq"} the logistic regression is based on GLM (frequentist), if \code{method = "bayes"} a sample from
#' the posterior distribution of a logistic regression model using a random walk Metropolis algorithm is generated (see MCMClogit function).
#'
#' The argument \code{niter} requires a number. For the GLM regression (\code{method = "fq"}), a non-parametric bootstrap method consists
#' in generate B bootstrap samples, by resampling with replacement the original data. Then all statistics for each parameter
#' can be calculated from each bootstrap sample (median and confidence intervals).
#' For the \code{method = "bayes"}, the argument \code{niter} is related to the number of Metropolis iterations for the sampler.
#' @examples
#' data(matFish)
#'
#' gonad_mat = gonad_mature(matFish, varNames = c("total_length", "stage_mat"), inmName = "I",
#' matName = c("II", "III", "IV"), method = "fq", niter = 50)
#'
#' # 'niter' parameters:
#' gonad_mat$A_boot
#' gonad_mat$B_boot
#' gonad_mat$L50_boot
#' gonad_mat$out
#' @export


hello <- function() {
  print("Hello, world!")
}
