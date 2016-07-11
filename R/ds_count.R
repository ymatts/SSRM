# 完成?
ds_count <- function(graph, community, nodename=NULL) {

  ds.count.vec <- vector("integer", length(community$names))

  i <- 0
  for (node.name in community$names) {
    i <- i + 1

    communities.name.num <- community$membership
    names(communities.name.num) <- community$names

    # i番目のノードが属するコミュニティの番号
    com.num <- community$membership[[i]]

    # i番目のノードが属するコミュニティ以外のコミュニティに属するノードの名前の一覧
    tgt.node <- community$names[com.num != community$membership]

    each.node.community <- vector("integer", length(tgt.node))
    names(each.node.community) <- tgt.node
    for (ix in seq(1,length(tgt.node))) {
      each.node.community[[ix]] <- communities.name.num[tgt.node[[ix]]]
    }

    # i番目のノードが属するコミュニティ以外のコミュニティに属するノードのうち、経路があるものの一覧
    i.tgt <- get.all.shortest.paths(graph, node.name)$res
    i.tgt <- lapply(i.tgt, as_ids)
    i.tgt <- as.character(lapply(i.tgt, function(elem){elem[[length(elem)]]}))

    i.tgt.com.num <- lapply(i.tgt, function(elem){communities.name.num[elem]})
    i.tgt.com.num <- sapply(i.tgt.com.num, function(x){x})

    uniq.com.num.number <- length(unique(i.tgt.com.num))
    ds.count.vec[[i]] <- uniq.com.num.number
  }

  names(ds.count.vec) <- community$names

  if (is.null(nodename)) {
    result <- ds.count.vec
  } else {
    result <- ds.count.vec[nodename]
  }

  # 無向グラフの場合、ダブルカウントを除くため、1/2倍する
  if (!is.directed(graph)) {
    result <- 0.5*result
  }
  return(result)
}
