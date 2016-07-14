#' SSRM Class
#'
#' @name SSRM Class
#' @param igraph object
#' @return result
#' library(SSRM)
#' @author Yu Hatakeyama
#' @export
#'

SSRM <- R6::R6Class("SSRM",
  private = list(
    dispatcher = function(function_name, nodename=NULL) {
      if (is.null(nodename)) {
        code <- paste0("SSRM::", function_name, "(self$graph, self$community, self$nodename)")
        } else {
          code <- paste0("SSRM::", function_name, "(self$graph, self$community, nodename)")
          }
      result <- eval(parse(text=code))
      return(result)
      }
    ),

  public = list(
    graph = NULL,
    community = NULL,
    nodename = NULL,

  initialize = function(graph, community, nodename) {
    self$graph <- graph
    self$community <- community
    self$nodename <- nodename
    self$mediacy_score_nodes <- SSRM::mediacy_score(graph, community, nodename)
  },

  outsider = function() {
    result <- SSRM::outsider(self$graph, self$community)
    return(result)
  },

  leader = function(visualize=T) {
    result <- SSRM::leader(self$graph, self$community)
    return(result)
  },

  outermosts = function(visualize=T) {
    result <- SSRM::outermosts(self$graph, visualize)
    return(result)
  },

  mediator = function() {
    result <- SSRM::mediator(self$mediacy_score_nodes, self$community)
    return(result)
  },

  cbc = function(nodename) {
    result <- self$dispatcher("cbc", nodename)
    return(result)
  },

  lbc = function(nodename) {
    result <- self$dispatcher("lbc", nodename)
    return(result)
  },

  ds_count = function(nodename) {
    result <- self$dispatcher("ds_count", nodename)
    return(result)
  },

  nbc = function(nodename) {
    result <- self$dispatcher("nbc", nodename)
    return(result)
  }
  )
)
