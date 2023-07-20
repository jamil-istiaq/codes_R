sp <- read.table("sparrows.txt",header=TRUE)
cor(sp)
# calculate PCs on correlation matrix
(sp.prcomp <- prcomp(sp, center=TRUE, scale=TRUE ))
names(sp.prcomp)
#print only rotations/loadings/eigen vectors
sp.prcomp$rotation
# eigen values are variance so:
sp.prcomp$sdev^2
#Scree plots
screeplot(sp.prcomp)
screeplot(sp.prcomp, type="lines")


#Visualise PCs
load = sp.prcomp$rotation
sorted.loadings = load[order(load[,1]),1]
Main="Loadings Plot for PC1" 
xlabs="Variable Loadings"
dotchart(sorted.loadings,main=Main,xlab=xlabs,cex=1.5,col="red")

# DotPlot PC2
sorted.loadings = load[order(load[,2]),2]
Main="Loadings Plot for PC2"
xlabs="Variable Loadings"
dotchart(sorted.loadings,main=Main,xlab=xlabs,cex=1.5,col="red")

# Now draw the BiPlot
biplot(sp.prcomp,cex=c(1,0.7))
biplot(sp.prcomp,choices=3:4, cex=c(1,0.7))
#### Note: cex=number indicating the amount by which plotting text and symbols 
# should be scaled relative to the default. 1=default, 1.5 is 50% larger, 
# 0.5 is 50% smaller, etc. 


#Individual scores on each PC (PC1 and PC2 are coordinates for biplot)
(scores<-(sp.prcomp$x))

#Raw scores and loadings are standardised by the PC stdev/sqrt(n).
#the following code will produce the same biplot as above
scores<-sp.prcomp$x
n <- NROW(scores)
step1 <- sp.prcomp$sdev[1:2]
step2 <- step1 * sqrt(n)
yy<-t(t(sp.prcomp$rotation[, 1:2]) * step2)
xx<-t(t(scores[, 1:2])/step2)
biplot(xx,yy)

#test if PC's are correlated
cor(sp.prcomp$x[,1],sp.prcomp$x[,2])

##using princomp
(sp.princomp<-princomp(sp, cor = TRUE))
names(sp.princomp)
sp.princomp$loadings


##compare methods
?princomp
(sp.princomp<-princomp(sp, cor = TRUE))
(sp.princomp<-princomp(sp, cor=FALSE))
(sp.prcomp <- prcomp(sp, center=TRUE, scale=TRUE ))
(sp.prcomp <- prcomp(sp, center=TRUE ))
