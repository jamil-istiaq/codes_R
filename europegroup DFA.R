eeg <- read.table("europegroup.txt", header=TRUE)
str(eeg)
head(eeg)
attach(eeg)
library(MASS)
(eeg.lda<-lda(Group~AGR+MIN+MAN+PS+CON+SER+FIN+SPS, data=eeg)) #defines DF's
group.pred<-predict(eeg.lda, eeg)
table(eeg$Group, group.pred$class)
##correlations between original data and predicted for each DF
(original<-eeg[c(2:10)])
(dfa<-group.pred$x)
cor(original,dfa)

(eeg.lda<-lda(Group~AGR+MIN+MAN+PS+CON+SER+FIN+SPS, prior=c(0.15,0.05, 0.3, 0.5), data=eeg))
