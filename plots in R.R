sp <-read.table("sparrow2.txt", header=TRUE)
head(sp)
tail(sp)
attach(sp)

##base plot
plot(sp[,2:6])

library(car)
scatterplot(Extent~Length | Survival, xlab="Total Length (mm)", ylab="Alar Extent (mm) ", reg.line=F, smoother=F) #old code
warnings()
?scatterplot()
scatterplot(Extent~Length | Survival, xlab="Total Length (mm)", ylab="Alar Extent (mm) ", regLine=FALSE, smooth=FALSE) #new code


scatterplotMatrix(sp[2:6])
library(scatterplot3d)
scatterplot3d(Extent,Length,Humerus, main="3D Scatterplot", color=Survival)
library(rgl)
plot3d(Extent,Length,Humerus, col="red", size=3) 
#to save rgl image
rgl.postscript("sparrow.pdf","pdf", drawText=FALSE)  #did not work well at all - picture was bad
snapshot3d("sparrow.png","png") #only save as png, works well
?snapshot3d
??rgl.postscript

tools::package_dependencies(recursive = TRUE)$rgl
install.packages('3d')


##plot multiple plots per plot window
par(mfrow = c(2,1))
plot(Extent~Length)
plot(Head~Length)
par(mfrow = c(1,2))
par(mfrow = c(1,1))

#Profileplot using function - plots individuals(see R file)
library(RColorBrewer)
names <- c("Length","Extent","Head","Humerus","Sternum")
mylist <- list(Length, Extent, Head, Humerus, Sternum)
makeProfilePlot(mylist,names)
detach(sp)

#can get the same using matplot function - no need for so much code and specialsed makeProfilePLot function
?matplot
matplot(sp[,2:7], type='l', ylab='Measurement', xlab='Index')
#####################new data file########################
mf <-read.table("mandiblefull.dat", header=TRUE)
attach(mf)
head(mf)
str(mf)
#calculate means and standard deviation by group using function (see R file)
printMeanAndSdByGroup(mf[4:11],mf[2])

#copy means to a text file
mfmeans <-read.table("mfmeans.txt", header=TRUE)
library(reshape2)
(mfmeans_melt<-melt(mfmeans, id=c("Group")))
attach(mfmeans_melt)
##plot varaible profiles my group
library(ggplot2)
ggplot(mfmeans_melt, aes(variable, value, colour=as.factor(Group))) +  geom_point(size=3)+ geom_line(aes(group=Group)) 
str(mfmeans_melt)   
detach(mf)
detach(mfmeans_melt)    
#############new data file#################################
# starplot display requires one row af data per category
(td <-read.table("twdecade.dat", header=TRUE))
head(td)

stars( td[,2:4], draw.segments=TRUE,  labels =(td[,1]),key.loc=c(7,2), main="Toowoomba weather by Decade" )

##to save a plot at higher resolution
jpeg(filename="sparrow.jpeg", width=3.5, height=5, units='in', res=300)
plot(sp[,2:6])
dev.off()
?jpeg()

library(rgl)
