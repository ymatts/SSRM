#' Calculating leader
#'
#' @name leader
#' @param igraph object
#' @return result
#' @author Yu Hatakeyama
#' @export
#'

#leadersに属するノード(近接中心性に関する頻度が正規分布であると仮定したときの上2sigma範囲外)
leader <- function(graph, visualize=T) {
  center <- closeness(graph, mode="all", normalized=T)
  result <- center[center > (mean(center) + 2*sd(center))]

  if (visualize) {
    visualize.func(graph)
    }

  return(result)
}
