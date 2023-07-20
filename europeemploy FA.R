ee <- read.table("europeemploy.txt", header=TRUE)
head(ee)
str(ee)

(cmat <- round(cor( ee ), digits=3))

##to find eigen values and vectors as in table 7.2 from PCA
ee.prcomp <- prcomp(ee, center=TRUE, scale=TRUE )
vec<-round(t(ee.prcomp$rotation),digits=3)
# eigen values are variance so:
val<-round(t(ee.prcomp$sdev^2), digits=3)
cbind(t(val),vec)

#factanal function using 4 factors and no rotation
(ee.fa4 <- factanal(ee, factors=4, rotation="none" ))

# default R does not display loadings below 0.1 so use:
print(ee.fa4$loadings,cutoff=0.5)

# calculate communalities
(ee.comm<-1 - ee.fa4$uniqueness)


# Varimax rotation
(ee.fa4r <- factanal(ee, factors=4, rotation="varimax" ))
print(ee.fa4r$loadings,cutoff=0.5)
names(ee.fa4r)


# calculate communalities
(ee.comm<-1 - ee.fa4$uniqueness)
rotation="varimax"

# plot variable loadings for factor 1 and factor 2
load <- ee.fa4r$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(ee),cex=.7) # add variable names 
str(ee)

# calcualte scores
names(ee.fa4r)
ee.fa4r$scores
#no 'scores' name available
#to get scores we need to inlcude scores statement in original model
ee.fa4r <- factanal(ee, factors=4, scores="regression", rotation="varimax" )
names(ee.fa4r)
ee.fa4r$scores
# plot country scores for factor 1 and factor 2
cor(ee.fa4r$scores[,1], ee.fa4r$scores[,2])
ee%*%loadings(ee.fa4r)
ee.fa4p <- factanal(ee, factors=4, scores="regression", rotation="promax" )
cor(ee.fa4p$scores[,1], ee.fa4p$scores[,2])


load <- ee.fa4r$scores[,1:2]
plot(load)
plot(load,type="n") # set up plot
text(load,labels = row.names(ee),cex=.7) # add variable names 

#regression on components - example only - no GDP in the dataset
ee.lm<-lm(GDP~ee.fa4r$scores)


library(psych)
corMat<-cor(ee)
solution1 <- fa(r = corMat, nfactors = 4,  rotate = "varimax", fm = "pa")
print(solution1$loadings,cutoff=0.5)
solution2 <- fa(r = corMat, nfactors = 4, rotate = "varimax", fm = "ml")
print(solution2$loadings,cutoff=0.5)

##to produce chhsq test need to give n.obs so that the GofF test can reproduce the cor matrix
(solution2 <- fa(r = corMat, nfactors = 4, n.obs=30, rotate = "varimax", fm = "ml"))
solution2$Structure


# find country scores 4 factors in solution 2
scores<-(factor.scores(ee, solution2, method = c("tenBerge")))
# plot country scores factors 1 and 2 in solution 2
load <- scores$scores[,1:2]
plot(load,type="n") # set up plot
text(load,labels = row.names(ee),cex=.7) # add variable names 


##plots
plot(solution2)
fa.diagram(solution2)

##structure and pattern matrices using fa in Psych models
print(solution2$Structure,cutoff=0.5)
print(solution2$loadings,cutoff=0.5)
solution3 <- fa(r = corMat, nfactors = 4, rotate = "promax", fm = "ml")
print(solution3$Structure,cutoff=0.5)
print(solution3$loadings,cutoff=0.5)
names(solution3)


# plot factor 1 by factor 2
load <- solution2$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(ee),cex=.7) # add variable names 


# use PCA to determine number of factors in 'factanal' analysis
ee.prcomp <- prcomp(ee, center=TRUE, scale=TRUE )
names(ee.prcomp)
#print only rotations/loadings
ee.prcomp$rotation
# eigen values
round(ee.prcomp$sdev^2, digits=3)
screeplot(ee.prcomp, type="lines")

#Parallel analysis to choose number of components - added to lecture
# An alternative way to determine the number of factors is to compare the solution to random
# data with the same properties as the real data set. If the input is a data matrix, the
# comparison includes random samples from the real data, as well as normally distributed
# random data with the same number of subjects and variables. 
library(psych)
set.seed(124)
pa<-fa.parallel(ee, fm="ml", fa="pc", n.iter=500)
pa<-fa.parallel(ee, fm="ml", fa="pc", n.iter=500, error.bars=TRUE)
?psych::fa.parallel
names(pa)
pa$pc.sim
pa$pc.values
pa$ncomp
pa.out<-pa$values
tail(pa.out)
quants <- c(0.95)
apply( pa.out[,10:18], 2 , quantile , probs = quants )
?apply
##################################################################################
