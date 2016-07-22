#' Calculating LBC
#'
#' @name lbc
#' @param igraph object
#' @return result
#' @author Yu Hatakeyama
#' @export
#'

# 要見直し
lbc <- function(graph, community, nodename=NULL) {
  #CBC・LBCとの共通部分###########################################################
  short.path.mat <- shortest.paths(graph)
  all.node.comb.bi <- apply(short.path.mat, 1, function(x){ifelse(x != 0, 1, 0)})
  all.node.comb.dist.sum <- apply(all.node.comb.bi, 1, sum)
  ################################################################################

  outsider.mem <- outsider(graph, community)
  for (elem in outsider.mem) {
    all.node.comb.dist.sum[elem] <- 0
  }

  leader.names <- names(SSRM::leader(graph, visualize=F))
  leader.num <- length(leader.names)

  if (leader.num == 0) {
    stop("Leaders don't exist.")
  }

  leader.comb.names <- combinations(leader.num, 2, leader.names)
  leaders.path <- apply(leader.comb.names, 1, function(x){get.all.shortest.paths(graph, x[[1]], x[[2]])$res})

  all.nodes <- as_ids(V(graph))
  lpath.conter <- vector("integer", length(all.nodes))
  names(lpath.conter) <- all.nodes

  i <- 0
  for (elem in all.nodes) {
    i <- i +1
    for (ori in leaders.path) {
      ifelse(elem %in% as_ids(ori[[1]]), lpath.conter[[i]] <- lpath.conter[[i]]+1, lpath.conter[[i]])
    }
  }
  result <- ifelse(is.null(nodename), lpath.conter, lpath.conter[nodename])
  return(result)
}
