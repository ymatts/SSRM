#' Visualize each role of nodes.
#'
#' @name plot_roles
#' @param igraph object
#' @return None
#' @author Yu Hatakeyama
#' @export
#'
#'

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


.color_selector <- function(graph, role) {

  if (role == "outsider") {
    V(graph)$color <- "#008000"
  } else if (role == "leader") {
    V(graph)$color <- "#0000ff"
  } else if (role == "outermost") {
    V(graph)$color <- "#800080"
  } else if (role == "mediator") {
    V(graph)$color <- "#ff0000"
  } else {
    stop("Such a role doesn't exist!")
  }

  V(graph)$color <- rep(color, length(all_node_names))

  #Silver
  V(graph)$color[vertex_attr(graph)[role_node] == 0] <- "#c0c0c0"
}


plot_roles <- function(graph, community, file_name=NULL, save_dir=NULL) {

  outsider_nodes <- SSRM::outsider(graph, community)
  leader_nodes <- SSRM::leader(graph, visualize=F)
  outermost_nodes <- SSRM::outermost(graph, visualize=F)
  mediator_nodes <- SSRM::mediator(SSRM::mediacy_score(graph, community, nodename=NULL), community)

  V(graph)$OutsiderNode <- .attr_dipatcher(graph, outsider_nodes)
  V(graph)$LeaderNode <- .attr_dipatcher(graph, leader_nodes)
  V(graph)$OutermostNode <- .attr_dipatcher(graph, outermost_nodes)
  V(graph)$MeditorNode <- .attr_dipatcher(graph, mediator_nodes)

  graph$color <- .color_selector(graph, "outsider")
  graph$color <- .color_selector(graph, "leader")
  graph$color <- .color_selector(graph, "outermost")
  graph$color <- .color_selector(graph, "mediator")

  par(new=F)

  if(save) {
    pdf(filename=paste0(save_dir, "/", file_name))
  }

  plot(graph, vertex.size=10, #ノードの大きさ
     vertex.shape="circle", #ノードの形
     vertex.label=V(graph)$name, #ノード属性nameをノードラベルにする。
     vertex.label.color="gray50", #ノードのラベルの色
     vertex.label.font=2, #ノードのラベルのスタイル 1: 普通, 2: 太字, 3: 斜体, 4: 太字斜体, 5: ギリシャ文字
     vertex.frame.color="white", #ノードの枠の色
     vertex.label.cex=0.8, #ノードラベルの文字サイズ
     edge.width=E(graph)$weight, #エッジ属性weightをエッジの太さとする
     edge.color="gray80", #エッジの色
     layout=layout.fruchterman.reingold) #ネットワークのレイアウト手法

  par(new=F)

  if(save) {
    dev.off()
  }
}