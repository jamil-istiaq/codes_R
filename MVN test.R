mvn1 <-read.table("mvntest.dat", header=TRUE)
mvn1 

##QQplots
par(mfrow = c(1,2))
qqnorm(mvn1$sales, ylab="sales sample quantiles")
qqnorm(mvn1$profit, ylab="profit sample quantiles")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
hist(mvn1$sales)
hist(mvn1$profit)
par(mfrow = c(1,1))
##calculating the univariate QQplot by hand
(n<-length(mvn1$sales))
(u<-((1:n)/(n+1)))  ##create as many quantiles as there are obs
(z<-qnorm(u))##find the z-scores for the quantiles
y<-mvn1$sales
(y_sorted<-sort(y))
(xy<-cbind(y_sorted, z))
plot(z, y_sorted)
##could also calculate z-scores of observed data and plot sample z 
#against theoretical z
hist(mvn1$sales)

#convert dataframe to matrix
dat<-as.matrix(mvn1)
dat
dat.cov<-var(dat)
dat.cov
# mahalanobis distance measures distance of single observations from centre 
# of population (defined by centroid of the means of all variables).
dist<-(mahalanobis(dat, colMeans(dat), dat.cov))  
#this is the squared mahal distance which follows a chi-sq distribution if the data is normal
dist
(n<-length(dist))
(u<-((1:n)-0.5)/n)
#why subtract 0.5?
#(u1<-(1:n)/n)
#(p<-qchisq(u1, 2))
#ge and Inf returned when trying to use qchisq function for a vlaue of 1
#could have subtracted 0.25 instead - doesn't matter, subtract anything just so deosn't end on !
#(u<-((1:n)-0.25)/n)
(p<-qchisq(u, 2))
dist_sorted<-sort(dist)
xy<-cbind(dist_sorted, p)
xy
plot(dist_sorted, p)
abline(0,1)

plot(p, dist_sorted)
abline(0,1)

##MVN package using the mvn dataframe
library(MVN)
?mvn
#univariate tests of normality for MVN package v5.0
#can only pick one type of plt at a time - console output is 
#Shaprio-wilks univariate normaility test unless another is chosen
mvn(mvn1,mvnTest = "mardia", univariateTest= "SW", univariatePlot="qqplot")
mvn(mvn1,mvnTest="mardia", univariateTest="SW",univariatePlot="histogram", desc=FALSE)
par(mfrow = c(1,2))
mvn(mvn1, mvnTest="mardia", univariateTest="SW", multivariatePlot="persp", desc=FALSE) #mardia is default test - does not change plot
mvn(mvn1, mvnTest="mardia", univariateTest="SW", multivariatePlot="contour", desc=FALSE) #mardia is default test - does not change plot
par(mfrow = c(1,1))
mvn(mvn1, mvnTest="mardia", univariateTest="SW",multivariatePlot="qq", desc=FALSE)
mvn(mvn1, mvnTest="hz", desc=FALSE)
mvn(mvn1, mvnTest="royston", desc=FALSE)

#testing for outliers
result<-mvn(mvn1, multivariateOutlierMethod="quan", multivariatePlot="qq") 
result<-mvn(mvn1, multivariateOutlierMethod="adj", multivariatePlot="qq") 
names(result)
result$multivariateNormality
(p<-qchisq(0.975, 2))
result<-mvn(mvn1, multivariateOutlierMethod="quan", multivariatePlot="qq", showNewData = TRUE) 
names(result)
result$newData
mvn2<-result$newData


#to change plot axis labels we need to hack the system
?mvn
MVN:::mvn
MVN:::mardia
MVN:::uniNorm

#Save code for mvn to a script file called 'hackMVN'
#change the plot code axis labels then run
source("hackMVN.R")
hackMVN(mvn1, mvnTest="mardia", multivariatePlot="qq")
###____________________________________________________________###

##To get back to version 4.0 of MVN package
# library(utils)
# remove.packages("MVN")
# require(devtools)
# install_version("MVN", version = "4.0", repos = "http://cran.us.r-project.org")
# ?remove.packages

#If the code above doesn't work try:
#remove.packages("MVN")
#to install version 4.0 use the zip file 


