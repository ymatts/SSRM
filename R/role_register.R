#' Adding property of roles to SSRM objects
#'
#' @name role_register
#' @param igraph object
#' @return None
#' @author Yu Hatakeyama
#' @export
#'
#'

role_register <- function(graph, community, role=NULL) {
  if (!is.null(role)) {
    if (role == "outsider") {
      outsider_nodes <- SSRM::outsider(graph, community)
      V(graph)$OutsiderNode <- .attr_dipatcher(graph, outsider_nodes)
    }

    if (role == "outsider") {
      leader_nodes <- SSRM::leader(graph, visualize=F)
      V(graph)$LeaderNode <- .attr_dipatcher(graph, leader_nodes)
    }

    if (role == "outermost") {
      outermost_nodes <- SSRM::outermost(graph, visualize=F)
      V(graph)$OutermostNode <- .attr_dipatcher(graph, outermost_nodes)
    }

    if (role == "mediator") {
      mediator_nodes <- SSRM::mediator(SSRM::mediacy_score(graph, community, nodename=NULL), community)
      V(graph)$MediatorNode <- .attr_dipatcher(graph, mediator_nodes)
    }
  } else {
    outsider_nodes <- SSRM::outsider(graph, community)
    leader_nodes <- SSRM::leader(graph, visualize=F)
    outermost_nodes <- SSRM::outermost(graph, visualize=F)
    mediator_nodes <- SSRM::mediator(SSRM::mediacy_score(graph, community, nodename=NULL), community)

    V(graph)$OutsiderNode <- .attr_dipatcher(graph, outsider_nodes)
    V(graph)$LeaderNode <- .attr_dipatcher(graph, leader_nodes)
    V(graph)$OutermostNode <- .attr_dipatcher(graph, outermost_nodes)
    V(graph)$MediatorNode <- .attr_dipatcher(graph, mediator_nodes)
  }
  return(invisible(graph))
}


.attr_dipatcher <- function(graph, role_nodes) {
  all_node_names <- names(V(graph))
  zero_nodes <- vector("integer", length(all_node_names))
  names(zero_nodes) <- all_node_names

  ix <- 1
  for (elem in names(role_nodes)) {
    zero_nodes[elem] <- role_nodes[[ix]]
    ix <- ix + 1
  }
  return(zero_nodes)
}
