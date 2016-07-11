library("R6")

SSRM <- R6Class(
  classname = "SSRM",
  
  initialize = function(graph, community) {
    self$graph <- graph
    self$community <- community
  },
  
  outsider = outsider
)