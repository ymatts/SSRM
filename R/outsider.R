outsider <- function(graph, community) {
  com.sum <- table(community$membership)
  result <- names(V(graph)[com.sum == 1])
  return(result)
}