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
  mediator.set <- vector("character")
  connected.coms <- vector("numeric")

  community.count <- length(communities(community))

  ix <- 1
  while (length(connected.coms) < community.count) {
    n <- names(mediacy_score_nodes)[[ix]]
    omit.com.num <- as.integer(com.name.memb[names(com.name.memb) == n])
    other.coms <- all.com[-omit.com.num]

    for (each.com in other.coms) {
      if (!(each.com %in% connected.coms)) {
        mediator.set <- append(mediator.set, n)
        connected.coms <- append(connected.coms, each.com)
      }
    }
    ix <- ix + 1
  }

  names(connected.coms) <- mediator.set
  return(connected.coms)
}
