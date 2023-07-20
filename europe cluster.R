ec <- read.table("europeemploy.txt",header=TRUE)
head(ec)
attach(ec)
#standardise the data
ec.std <- scale(ec)
head(ec.std)
#scale=standardising
(2.6-mean(ec$AGR))/sqrt(var(ec$AGR))


#nearest neighbour
es.hc <- hclust(dist(ec.std), method="single")
plot(es.hc, hang=-1)
#extracting four clusters
cutree(es.hc, k=4)
# Sorting can make the clusters easier to identify
sort ( cutree(es.hc, k=4) )

#group average
es.hc.g <- hclust(dist(ec.std), method="average")
plot(es.hc.g, hang=-1)


# Ward's method
es.hc.w <- hclust(dist(ec.std), method="ward.D")
plot(es.hc.w, hang=-1)
sort ( cutree(es.hc.w, k=4) )


#partitioning or k-means method
#2 groups
ec.km2 <- kmeans( ec, centers=2)
row.names(ec)[ec.km2$cluster==1]
row.names(ec)[ec.km2$cluster==2]

# 6 groups
ec.km6 <- kmeans( ec, centers=6)
row.names(ec)[ec.km6$cluster==1]
row.names(ec)[ec.km6$cluster==2]
row.names(ec)[ec.km6$cluster==3]
row.names(ec)[ec.km6$cluster==4]
row.names(ec)[ec.km6$cluster==5]
row.names(ec)[ec.km6$cluster==6]
