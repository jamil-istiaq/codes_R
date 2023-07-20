sv<-read.table("soilveg.txt", header=FALSE)
head(sv)
names(sv)=c("Case", "X1", "X2", "X3", "X4", "Y1", "Y2", "Y3", "Y4") #adding a header row
head(sv)
tail(sv)
str(sv)
## the missing value in case 152 is causing X1 to be read as factor. Change the data import line

sv<-read.table("soilveg.txt", header=FALSE, na.strings = c("m")) #recognises 'm; as missing
names(sv)=c("Case", "X1", "X2", "X3", "X4", "Y1", "Y2", "Y3", "Y4") #adding a header row
tail(sv)
str(sv) #sample size still 152 but X1 is now integer and 'm; changed to system missing NA.
sv<-na.omit(sv) #overwriting the sv dataframe with data excluding na
str(sv)   ##n=151

##data standardised mean=0 and stdev=1
(svst<- scale(sv[2:9]))
soil <- svst[, 1:4]
veg <- svst[, 5:8]
cor(soil,soil)
cor(soil,veg)
cor(veg,veg)


library(yacca)
svst.cca <- cca(soil, veg)
summary(svst.cca)
F.test.cca(svst.cca)
names(svst.cca)
plot(svst.cca$corr,type="b")
svst.cca$xcrosscorr
svst.cca$ycrosscorr


library(CCA)
(svst1.cc <- cc(soil, veg))
comput(soil, veg, svst1.cc)

plt.cc(svst1.cc)
##to give variable labels

plt.cc(svst1.cc,d1=1,d2=2,type="b",var.label=TRUE)


##to download source code so that I can see the code for plt.cc function and then untar it
untar(download.packages(pkgs = "CCA",
                        destdir = ".",
                        type = "source")[,2])


plt.cc
plt.var
##copy function code and edit code - see files plt.var1.R and plt.cc1.R
## see instrucitons in Tutorial 8 solutions
?cc
plt.cc1(svst1.cc,d1=1,d2=2,type="b",var.label=TRUE)
#******************************************************************
###Instead of standardising the original variables you can use code
##below to get the standardised coeficients:

soil <- sv[, 2:5]
veg <- sv[, 6:9]
sv.cca <- cc(soil, veg)
s1 <- diag(sqrt(diag(cov(soil))))
s1 %*% sv.cca$xcoef
##****************************************************************
