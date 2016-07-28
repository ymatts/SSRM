#' Visualize each role of nodes.
#'
#' @name plot_roles
#' @param igraph object
#' @return None
#' @author Yu Hatakeyama
#' @export
#'
#'


plot_roles <- function(role, graph, community, save=F, file_name=NULL, save_dir=NULL) {

  graph <- SSRM::role_register(graph, community, role)
  graph <- .color_selector(graph, role)


  par(new=F)
  if(save) {
    pdf(filename=paste0(save_dir, "/", file_name))
  }

  #Roleの数値も表示するように実装を加える
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


.color_selector <- function(graph, role) {

  if (role == "outsider") {
    color <- "#008000"
    role_name <- "OutsiderNode"
  } else if (role == "leader") {
    color <- "#0000ff"
    role_name <- "LeaderNode"
  } else if (role == "outermost") {
    color <- "#800080"
    role_name <- "OutermostNode"
  } else if (role == "mediator") {
    color <- "#ff0000"
    role_name <- "MediatorNode"
  } else {
    stop("Such a role doesn't exist!")
  }

  all_node_names <- names(V(graph))
  V(graph)$color <- rep(color, length(all_node_names))

  #Silver
  V(graph)$color[vertex_attr(graph)[role_name][[1]] == 0] <- "#c0c0c0"

  return(graph)
}
