#' Calculating CBC
#'
#' @name cbc
#' @param igraph object
#' @return result
#' @author Yu Hatakeyama
#' @export
#'

cbc <- function(graph, community, nodename=NULL) {
  #CBC・LBCとの共通部分###########################################################
  short.path.mat <- shortest.paths(graph)

  #距離は関係が無い
  all.node.comb.bi <- apply(short.path.mat, 1, function(x){ifelse(x != 0, 1, 0)})
  ################################################################################

  #始点のノードをラベル、終点のノードを要素とする
  se.comb.list <- vector("list", length(community$names))
  names(se.comb.list) <- community$names

  cbc.vec <- vector("integer", length(community$names))
  names(cbc.vec) <- community$names

  i <- 0
  for (node.name in community$names) {
    i <- i + 1
    com.num <- community$membership[[i]]
    tgt.node <- community$names[com.num != community$membership]
    se.comb.list[[i]] <- tgt.node
    cbc.vec[[i]] <- sum(all.node.comb.bi[names(se.comb.list)[[i]], se.comb.list[[i]]])
  }

  if (is.null(nodename)) {
    result <- cbc.vec
  } else {
    result <- cbc.vec[nodename]
  }

  if (!is.directed(graph)) {
    result <- 0.5*result
  }
  return(result)
}
