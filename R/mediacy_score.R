# 結構遅いのでRCpp使う必要あるかも
mediacy_score <- function(graph, community, nodename=NULL) {
  if (is.null(nodename)) {
    result <- nbc(graph, community) * ds_count(graph, community)
  } else {
    result <- nbc(graph, community, nodename) * ds_count(graph, community, nodename)
  }
  return(result)
}
