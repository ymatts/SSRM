#サンプルデータ準備

library(SSRM)
library(igraph)
library(reshape2)

counts <- read.table("http://bowtie-bio.sourceforge.net/recount/countTables/wang_count_table.txt", row.names=1, header=TRUE)
counts <- counts[rank(- rowMeans(counts)) <= 100,]
counts.log <- log10(counts + 1)
#dim(counts.log)

cc <- cor(t(counts.log))
cc[upper.tri(cc)] <- NA
diag(cc) <- NA
cc.df <- melt(cc)
cc.df <- cc.df[!is.na(cc.df[,3]),]

cc.df.sig <- cc.df[abs(cc.df[,3]) > 0.75,]
g <- graph.data.frame(cc.df.sig[, 1:2], directed=F)

# グラフ描画パラメーターの調整
num.of.v <- length(V(g))
V(g)$size  <- rep(5, num.of.v)
V(g)$color <- rep("#E41A1C", num.of.v)
V(g)$shape <- rep("circle", num.of.v)
V(g)$label <- names(as.list(V(g)))
V(g)$label.cex   <- rep(0.5, num.of.v)
V(g)$label.color <- rep("black", num.of.v)

plot(g)

#sc <- spinglass.community(g)
wc <- walktrap.community(g)
dend <- as.dendrogram(wc)
plot(dend)


# SSRM package test
g.obj <- SSRM$new(g, wc)
g.obj$plot_community()

g.obj$outsider()
g.obj$leader()
g.obj$outermosts()
g.obj$mediator()
g.obj$cbc()
#g.obj$lbc()
g.obj$ds_count()
g.obj$nbc()
#g.obj$save_igraph(file="test.gexf")

#########
#http://deta.hateblo.jp/entry/2013/05/08/053707
g <- simplify(g, remove.multiple=TRUE, remove.loops=TRUE)
dcg <- decompose.graph(g)

g.cl <- clusters(g)

# 連結ネットワーク単位にコミュニティーと中心性を計算する
sp.df.all <- c() # コミュニティ分割結果データフレーム
for (i in 1:length(dcg)) {
  set.seed(1) # シードを固定
  sp <- spinglass.community(dcg[[i]]) # 焼きなまし法 グラフラプラシアンを使ってQ値が最大となるような分割を探す
  sp.df <- cbind(i, as.data.frame(sp$names), as.data.frame(sp$membership)) # データフレーム形式に変換
  sp.df.all <- rbind(sp.df.all, sp.df) # コミュニティーに結果を追加
}

colnames(sp.df.all) <- c("CommunityNumber", "NodeName", "Community") # 列名変更


#######################


# サンプルデータの用意
g.sample <- graph.full(5) %du% graph.full(5) %du% graph.full(5)  # 完全グラフを3つ作る
g.sample <- add.edges(g.sample, c(1, 6, 1, 11, 6, 11))  # ノード1,6間、1,11間、6,11間にエッジを追加する
plot(g.sample, layout = layout.lgl)  # プロットする



memb <- community.to.membership(g.sample, fc$merges, steps = which.max(fc$modularity) - 1)
V(g.sample)$color <- rainbow(length(memb$csize))[memb$membership + 1]
plot(g.sample, layout = layout.fruchterman.reingold, edge.arrow.size = 0.5)

