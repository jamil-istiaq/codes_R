es <-read.table("eskulls.txt", header=TRUE)
head(es)
str(es)
table(es$Period)

##the matrices for W and B were calculated in eskulls.R week 4
(F<-(solve(W))%*%B)
(eig<- eigen(F))      


#a plot
library(lattice)
head(es)
splom(es[,1:4], groups=es$Period)

#Linear DA (LDA) = DFA
library(MASS)
(es.lda<-lda(Period~MB+BH+BL+NH, data=es)) #defines DF's

##manova to test if difference between groups using DFs as dependent
#need to run lda.temp below first
es.manova1<-manova(cbind(LD1, LD2, LD3, LD4) ~ class, data=lda.temp)
summary(es.manova1, test="Pillai")
summary(es.manova1, test="Wilks")
summary(es.manova1, test="Roy")
summary(es.manova1, test="Hotelling-Lawley")


##use DFA model to predict membership
(period.pred<-predict(es.lda, es))  #predicts period 
table(es$Period, period.pred$class)  #compares true period to predicted 


# DFs with individuals grouped by original Period classifications
lda.temp<-data.frame(period.pred$x, class=es$Period)
xyplot(LD2~LD1, data=lda.temp, groups=class, 
auto.key=list(title="Period", space = "top", cex=1.0))

# DFs with individuals grouped by predicted Period classifications
lda.temp1<-data.frame(period.pred$x, class=period.pred$class)
xyplot(LD2~LD1, data=lda.temp1, groups=class, 
auto.key=list(title="Period", space = "top", cex=1.0))



##create training and test sets
library(ggplot2)
library(caret)
set.seed(107)
inTrain <- createDataPartition(y = es$Period, ## the outcome data are needed
                               p = .75, ## The percentage of data in the training set
                               list = FALSE)

estrain <- es[ inTrain,]
estest <- es[-inTrain,]
table(estrain$Period)
table(estest$Period)
 
estrain.lda<-lda(Period~MB+BH+BL+NH, data=estrain) #defines DF's based on training data
period.pred<-predict(estrain.lda, estest)  #predicts period for estest set
table(estest$Period, period.pred$class)  #compares true period to predicted for test group

