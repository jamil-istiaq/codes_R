cm <- read.table("congressman.txt", header=TRUE)
head(cm)
str(cm)

# metric MDS
(fit1 <- cmdscale(cm,eig=TRUE, k=2)) # k is the number of dim, eig=TRUE will give GoF
(fit2 <- cmdscale(cm,eig=TRUE, k=3))
(fit3 <- cmdscale(cm,eig=TRUE, k=4))
# plot solution
x <- fit1$points[,1]
y <- fit1$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(cm), cex=.7) 



##monotonic regression relationship
library(vegan)
(cmmds <- monoMDS(cm, k=3))
plot(cmmds,choices = c(1,2))





