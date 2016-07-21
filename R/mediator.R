#' Calculating med_extractor
#'
#' @name mediator
#' @param igraph object
#' @return result
#' @author Yu Hatakeyama
#' @export
#'


mediator <- function(mediacy_score_nodes, community) {
  com.name.memb <- membership(community)
  all.com <- sort(unique(wc$membership))

  mediacy_score_nodes <- sort(mediacy_score_nodes, decreasing=T)
  mediator.set <- vector("list")
  connected.coms <- vector("list")

  connected.coms.size <- length(connected.coms)
  community.count <- length(communities(community))

  ix <- 1
  while (connected.coms.size < community.count) {
    n <- mediacy_score_nodes[[ix]]

    omit.com.num <- as.integer(com.name.memb[n])
    other.coms <- all.com[-omit.com.num]
    for (each.com in other.coms) {
      if (!(each.com %in% connected.coms)) {
        mediator.set <- append(mediator.set, n)
        connected.coms <- append(connected.coms, each.com)
      }
    }
    ix <- ix + 1
  }
  mediator.set <- unlist(mediator.set)
  connected.coms <- unlist(connected.coms)

  result <- list(mediator.set, connected.coms)
  names(result) <- c("mediatorSet", "connectedCommunities")

  return(result)
}
