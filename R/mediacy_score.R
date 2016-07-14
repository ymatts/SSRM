#' Calculating Mediacy Score
#'
#' @name mediacy_score
#' @param igraph object
#' @return result
#' library(SSRM)
#' @author Yu Hatakeyama
#' @export
#'

# 結構遅いのでRCpp使う必要あるかも
mediacy_score <- function(graph, community, nodename=NULL) {
  if (is.null(nodename)) {
    result <- SSRM::nbc(graph, community) * SSRM::ds_count(graph, community)
  } else {
    result <- SSRM::nbc(graph, community, nodename) * SSRM::ds_count(graph, community, nodename)
  }
  return(result)
}
