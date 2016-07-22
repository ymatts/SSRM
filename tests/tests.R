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

g.obj$outsider()
g.obj$leader()
g.obj$outermosts()
g.obj$mediator()
g.obj$cbc()

#g.obj$lbc()

g.obj$ds_count()
g.obj$nbc()
