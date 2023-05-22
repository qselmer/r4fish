#' Title
#'
#' @param sp
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
getStockInfo  <- function(sp,
                          data = NULL,
                          ...)
  {
  if (sp %in% rownames(species)) {
    output <- as.list(species[sp, ])
  }
  else {
    stop("Incorrect value for 'sp'.")
  }
  return(output)
}
