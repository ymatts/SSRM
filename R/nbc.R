nbc <- function(graph, community, nodename=NULL) {
  #CBC・LBCとの共通部分###########################################################
  short.path.mat <- shortest.paths(graph)

  #距離は関係が無いので経路の有無によって二値化する
  all.node.comb.bi <- apply(short.path.mat, 1, function(x){ifelse(x != 0, 1, 0)})
  ################################################################################

  #始点のノードをラベル、終点のノードを要素とする
  se.comb.list <- vector("list", length(community$names))
  names(se.comb.list) <- community$names

  #コミュニティの名前をラベル、NBCの値を要素とする
  nbc.vec <- vector("integer", length(community$names))
  names(nbc.vec) <- community$names

  #コミュニティのサイズを求める
  com.num.number <- table(wc$membership)

  #コミュニティのサイズをラベル、コミュニティの番号を要素とする
  com.num.number.inv <- as.integer(names(com.num.number))
  names(com.num.number.inv) <- com.num.number

  #それぞれのノードが所属するコミュニティの番号
  each.com.name.num <- community$membership


  i <- 0
  for (elem in community$membership) {
    i <- i + 1

    #それぞれのノードが属するコミュニティのサイズを格納
    each.com.name.num[[i]] <- as.integer(names(com.num.number.inv[com.num.number.inv == community$membership[[i]]]))
  }

  #ノードの名前をラベルとする
  names(each.com.name.num) <- community$names

  i <- 0
  for (node.name in community$names) {
    i <- i + 1

    #i番目のノードが属するコミュニティの番号
    com.num <- community$membership[[i]]

    #i番目のノードが属するコミュニティ以外のコミュニティに属するノードの名前の一覧
    tgt.node <- community$names[com.num != community$membership]

    #始点(i番目のノード)のノードをラベル、これに対応する異なるコミュニティに属する終点のノード群を要素とする
    se.comb.list[[i]] <- tgt.node

    #始点(i番目のノード)に対応する終点のノードのベクトル(二値化しているため、全て1)(I_p(p,v))
    tmp.vec <- all.node.comb.bi[names(se.comb.list)[[i]], se.comb.list[[i]]]

    #始点のノード(i番目のノード)の名前
    st.node.name <- names(se.comb.list)[[i]]

    #始点のノード(i番目のノード)が属するコミュニティのサイズ
    st.node.com.num.number <- each.com.name.num[st.node.name]

    #始点のノード(i番目のノード)に対応する終点のノード群が属するコミュティのサイズ
    ed.node.com.num.number.vec <- each.com.name.num[names(tmp.vec)]

    #始点のノード(i番目のノード)が属するコミュニティのサイズと終点のノード群が属するコミュティのサイズを比較
    #小さい方を残す
    ed.node.com.num.number.vec[ed.node.com.num.number.vec > st.node.com.num.number] <- st.node.com.num.number

    #I_p(p,v)をコミュティサイズの小さい方で割る
    tmp.vec <- tmp.vec/ed.node.com.num.number.vec

    #総和をとる
    nbc.vec[[i]] <- sum(tmp.vec)
  }

  if (is.null(nodename)) {
    result <- nbc.vec
  } else {
    result <- nbc.vec[nodename]
  }

  #無向グラフの場合、ダブルカウントを除くため、1/2倍する
  if (!is.directed(graph)) {
    result <- 0.5*result
  }
  return(result)
}
