#' Plots communities.
#'
#' @name plot_community
#' @param igraph object
#' @return None
#' @author Yu Hatakeyama
#' @export
#'

plot_community <- function(graph, community, save=F, file_name=NULL, save_dir=NULL) {

  if ((is.null(V(graph)$CommunityName)) || (is.null(V(graph)$color))) {
    V(graph)$CommunityName <- community$membership
    com.nums <- as.integer(V(graph)$CommunityName)
    V(graph)$color <- rainbow(max(com.nums))[com.nums]
  }

  par(new=F)

  if(save) {
    pdf(filename=paste0(save_dir, "/", file_name))
  }

  plot(graph, vertex.size=10, #ノードの大きさ
       vertex.shape="circle", #ノードの形
       vertex.label=V(g)$name, #ノード属性nameをノードラベルにする。
       vertex.label.color="gray50", #ノードのラベルの色
       vertex.label.font=2, #ノードのラベルのスタイル 1: 普通, 2: 太字, 3: 斜体, 4: 太字斜体, 5: ギリシャ文字
       vertex.frame.color="white", #ノードの枠の色
       vertex.label.cex=0.8, #ノードラベルの文字サイズ
       edge.width=E(g)$weight, #エッジ属性weightをエッジの太さとする
       edge.color="gray80", #エッジの色
       layout=layout.fruchterman.reingold) #ネットワークのレイアウト手法
  par(new=F)

  if(save) {
    dev.off()
  }
}
