#' Calculating Outsider
#' 
#' @name outsider
#' @param igraph object
#' @return result
#' library(SSRM)
#' @author Yu Hatakeyama
#' @export
#' 

outsider <- function(graph, community) {
  com.sum <- table(community$membership)
  result <- names(V(graph)[com.sum == 1])
  return(result)
}