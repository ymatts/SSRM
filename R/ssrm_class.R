#' SSRM Class
#'
#' @name SSRM Class
#' @param igraph object
#' @author Yu Hatakeyama
#' @export
#'

set.seed(20160728)

SSRM <- R6::R6Class("SSRM",
  private = list(
    dispatcher = function(function_name, graph, community, nodename) {
      code <- paste0("SSRM::", function_name, "(graph, community, nodename)")
      result <- eval(parse(text=code))
      return(result)
      }
    ),

  public = list(
    graph = NULL,
    community = NULL,
    nodename = NULL,
    mediacy_score_nodes = NULL,

    OutsiderNode = NULL,
    LeaderNode = NULL,
    OutermostNode = NULL,
    MeditorNode = NULL,

  initialize = function(graph, community, nodename=NULL) {

    com.nums <- as.integer(community$membership)
    V(graph)$CommunityNumber <- com.nums
    V(graph)$color <- rainbow(max(com.nums))[com.nums]

    self$graph <- graph
    self$community <- community
    self$nodename <- nodename
  },

  outsider = function(graph=self$graph, community=self$community) {
    result <- SSRM::outsider(graph, community)
    return(result)
  },

  leader = function(graph=self$graph, visualize=T) {
    result <- SSRM::leader(graph, visualize)
    return(result)
  },

  outermost = function(graph=self$graph, visualize=T) {
    result <- SSRM::outermost(graph, visualize)
    return(result)
  },

  mediator = function(mediacy_score_nodes=self$mediacy_score_nodes, community=self$community) {
    if (is.null(self$mediacy_score_nodes)) {
      self$mediacy_score_nodes <- SSRM::mediacy_score(graph, community, nodename)
      mediacy_score_nodes <- self$mediacy_score_nodes
    }
    result <- SSRM::mediator(mediacy_score_nodes, community)
    return(result)
  },

  cbc = function(graph=self$graph, community=self$community, nodename=self$nodename) {
    result <- private$dispatcher("cbc", graph, community, nodename)
    return(result)
  },

  lbc = function(graph=self$graph, community=self$community, nodename=self$nodename) {
    result <- private$dispatcher("lbc", graph, community, nodename)
    return(result)
  },

  ds_count = function(graph=self$graph, community=self$community, nodename=self$nodename) {
    result <- private$dispatcher("ds_count", graph, community, nodename)
    return(result)
  },

  nbc = function(graph=self$graph, community=self$community, nodename=self$nodename) {
    result <- private$dispatcher("nbc", graph, community, nodename)
    return(result)
  },

  role_register = function(graph=self$graph, community=self$community, role_name=NULL) {
    igraph_obj <- SSRM::role_register(graph, community, role_name)
    self$graph <- igraph_obj

    self$OutsiderNode <- V(igraph_obj)$OutsiderNode
    self$LeaderNode <- V(igraph_obj)$LeaderNode
    self$OutermostNode <- V(igraph_obj)$OutermostNode
    self$MeditorNode <- V(igraph_obj)$MeditorNode

    return(invisible(igraph_obj))
  },

  plot_community = function(graph=self$graph, community=self$community, save=F, file_name=NULL, save_dir=NULL) {
    SSRM:::plot_community(graph, community, save, file_name, save_dir)
  },

  plot_roles = function(role, graph=self$graph, community=self$community, save=F, file_name=NULL, save_dir=NULL) {
    SSRM:::plot_roles(role, graph, community, save, file_name, save_dir)
  },

  save_igraph = function(graph=self$graph, community=self$community, file_name=NULL, format="gexf", save_dir=NULL) {
    SSRM:::save_igraph(graph, community, file_name, format, save_dir)
  },

  mediacy_score = function(graph, community, nodename=NULL) {
   result <- SSRM::mediacy_score(graph=self$graph, community=self$community, nodename)
   return(result)
   }
  )
)

# Static Method
SSRM$.visualize_func <- function(graph=self$graph) {
  par(new=F)

  cent <- closeness(graph, mode="all", normalized=T)
  closeness_df <- data.frame(ClosenessCentrality=cent)

  h <- dpih(cent)
  bins <- seq(min(cent), max(cent)+h, by=h)

  g <- ggplot(closeness_df, aes(ClosenessCentrality)) +
    geom_histogram(aes(y=..density..), col="blue", fill="#619CFF", alpha=0.4, breaks=bins) + geom_density(col="red") +
    geom_rug(sides="b") + labs(title="Histogram for Closeness Centrarity")

  plot(g)

  par(new=F)
}
