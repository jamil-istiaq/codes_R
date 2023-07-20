mn <- read.table("mandible.txt", header=TRUE)
mn
# Standardize, then cluster
dist( scale(mn))
mn.hc <- hclust(dist( scale(mn) ), method="single")
plot(mn.hc, hang=-1)
