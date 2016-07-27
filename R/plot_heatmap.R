


# http://tomoshige-n.hatenablog.com/entry/2014/08/15/235510

library(ggplot2)
library(RColorBrewer)
library(reshape2)

dat <- matrix(rnorm(100, 3, 1), ncol=10)
names(dat) <- paste("X", 1:10)
dat2 <- melt(dat, id.var = "X1")
ggplot(dat2, aes(as.factor(Var1), Var2, group=Var2)) + geom_tile(aes(fill = value)) +
  geom_text(aes(fill = dat2$value, label = round(dat2$value, 1))) + scale_fill_gradient(low = "white", high = "red")
