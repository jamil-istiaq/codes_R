rd <- read.table("roads.txt", header=TRUE)
rd
str(rd)
plot(rd)

##monotonic regression relationship
library(vegan)
m <- monoMDS(rd, k=2, stress=1)
m
names(m)
plot(m)



