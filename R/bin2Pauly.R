bin2Pauly  <- function(vec , sp = "anchoveta",
                       stock = "nc"){
  marks <- r4fish:::.getMarks(sp = sp, stock = stock)
  byList <- marks %/% 1
  vec <- unname(unlist(vec))
  vec2 <- aggregate(x = vec, by = list(byList), sum)
  vec2 <- vec2$x
  nameList <- unique(byList) + 0.25
  names(vec2) <- nameList
  return(vec2)
}




