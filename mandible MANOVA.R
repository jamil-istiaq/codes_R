mf <-read.table("mandiblefull.dat", header=TRUE)
attach(mf)
str(mf)

#use a manova to see if there are differences in size across species.
#Group is numeric so use 'as.factor' to treat as categorical 
mf.manova1<-manova(cbind(X1, X2, X3, X4) ~ as.factor(Group), data=mf)
summary(mf.manova1)  #default test is Pillai's


#to subset and run individual comparisons in MANOVA:
mf.manova2<-manova(cbind(X1, X2, X3, X4) ~ as.factor(Group), data=mf, 
                   subset=as.factor(Group)%in% c("5", "4"))

##To run Hotellings on only 2 groups
library(DescTools)
G51<-subset(mf, Group=="5"|Group=="1")
(HotellingsT2Test(cbind(X1, X2, X3, X4, X5, X6, X7, X8,X9) ~ Group, data=G51))

##subset Males and females for the first 4 groups
table(Sex)
mf_sex<-subset(mf, Sex=="1"|Sex=="2")
mf_sex<-subset(mf, Group<5)
mf_sex
