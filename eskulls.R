# Clear memory
rm(list = ls())

es <-read.table("eskulls.txt", header=TRUE)
es
attach(es)
str(es)
order <- ordered(es$Period , levels=c("Early.P", "Late.P", "Dyn.12.13", 
                                      "Ptolemaic","Roman"))
boxplot(MB~order, data=es, xlab="Period", ylab="Maximum Breadth (mm)") 

##ANOVA MB=Period  by equations in Manly, Table 4.3
#subset groups
period1<-subset(es, Period=="Early.P", select=MB:NH)
period2<-subset(es, Period=="Late.P", select=MB:NH)
period3<-subset(es, Period=="Dyn.12.13", select=MB:NH)
period4<-subset(es, Period=="Ptolemaic", select=MB:NH)
period5<-subset(es, Period=="Roman", select=MB:NH)

#group means for MB
(mean<- mean(es$MB))
(mean1<-mean(period1$MB))
(mean2<-mean(period2$MB))
(mean3<-mean(period3$MB))
(mean4<-mean(period4$MB))
(mean5<-mean(period5$MB))

# Total SS 
(TMB<-sum((MB-mean)^2))

# Within SS
(WMB<-(sum((period1$MB-mean1)^2))+(sum((period2$MB-mean2)^2)) +(sum((period3$MB-mean3)^2))+
    (sum((period4$MB-mean4)^2)) + (sum((period5$MB-mean5)^2)))

# Between SS
(BMB<- TMB - WMB)

##check
#one-fator ANOVA on maximum breadth (MB)
(es.anova<-anova(lm(MB~Period)))
#############################################################

#MANOVA (MB, BH, BL, NH)=Period from equations
#create matrix of dependant variables
(Y=(cbind(es[,1:4] )))
#calculate group means for all dependent variables
(xbar<- (colMeans(Y)))   
(xbar1<- colMeans(period1))   
(xbar2<- colMeans(period2))  
(xbar3<- colMeans(period3))
(xbar4<- colMeans(period4))
(xbar5<- colMeans(period5))

#create matrix of period means
(period_means<-rbind(xbar1, xbar2, xbar3, xbar4, xbar5))

##exmaple of using sweep function to subtract vector element from each col 
(test<-sweep(Y,2,xbar,'-'))  
##in sweep 1= there is an xbar element for each row of Y (ie xbar holds 150 values)
##and 2=there is an xbar element for each col of Y (ie xbar has 4 values)  

#calculate SSCP(T)- total SS and Cross products
(T<-data.matrix(t(sweep(Y,2,xbar,'-')))%*% data.matrix(sweep(Y,2,xbar,'-')))
##or can also get T by:
n<-length(Y$MB)
(T<-cov(Y)*(n-1))  

#calculate SSCP(B) - between groups
(n<-(aggregate(MB ~ Year , data = es, FUN = length)))   
(n<-(n[,2]))  #select just second col of n
(pdiffs<-data.matrix(sweep(period_means,2,xbar,'-')))
(B<-data.matrix(t(pdiffs*n))%*% (pdiffs))

#calculate SSCP(W) - within groups
(W<-T-B)

##or the easy way
es.manova1<-manova(cbind(MB, BH, BL, NH) ~ as.factor(Period), data=es)
summary(es.manova1, test="Wilks")
(Wilks_lambda=det(W)/det(T)) #from equations
summary(es.manova1, test="Roy")
(Roys_max_root=eigen(solve(W) %*% B)) #from equations

summary(es.manova1)  ##pillai  is default
(pillai_V= (Roys_max_root$values[1]/( Roys_max_root$values[1]+1)) + 
            (Roys_max_root$values[2]/( Roys_max_root$values[2]+1)) +
            (Roys_max_root$values[3]/( Roys_max_root$values[3]+1)) + 
            (Roys_max_root$values[4]/( Roys_max_root$values[4]+1)))
 
    
summary(es.manova1, test="Hotelling-Lawley")    
(Lawely<-Roys_max_root$values[1] + Roys_max_root$values[2]+Roys_max_root$values[3] + Roys_max_root$values[4])


anova(es.manova1)
names(es.manova1)









