my_data <- read.table("usair_2023.dat", header=TRUE)
df<-as.data.frame(my_data)
str(df)
library(psych)
(df.fa2 <- factanal(df, factors=2, rotation="none" ))
print(df$vaccounted)
print(df$chisq)
print(df.fa2$loadings,cutoff=0.5)

diff_uniqueness <- df.fa2$uniqueness["temp"] - df.fa2$uniqueness["wind.speed"]
print(diff_uniqueness)

(df.fa2v <- factanal(df, factors=2, rotation="varimax" ))
print(df.fa2v$loadings,cutoff=0.5)

(df.comm<-1 - df.fa2$uniqueness)

library(psych)
set.seed(132)
pa<-fa.parallel(df, fm="ml",fa="pc", n.iter=500)
pa<-fa.parallel(df, fm="ml",fa="pc", n.iter=500, error.bars=TRUE)
names(pa)
pa$pc.sim
pa$pc.values
pa$ncomp
pa.out<-pa$values

quants <- c(0.95)
apply( pa.out[,1:5], 2 , quantile , probs = quants )


ir <-read.table("iris_2023.txt", header=TRUE)
str(ir)
summary(ir)
table(ir$species)
library(lattice)
splom(ir[,2:5], groups=ir$SPECIES)

library(caret)
set.seed(107)
irTrain <-createDataPartition(y = ir$SPECIES,
                               p = .75,
                               list = FALSE)

irtrain <- ir[ irTrain,]
irtest <- ir[-irTrain,]
table(irtrain$SPECIES)
table(irtest$SPECIES)

library(MASS)
species<-ir$SPECIES
sep_ln<-ir$SEPALLEN
sep_wt<-ir$SEPALWID
pet_ln<-ir$PETALLEN
pet_wt<-ir$PETALWID

library(MASS)
species<-ir$SPECIES
(irtrain.lda<-lda(species~sep_ln+sep_wt+pet_ln+pet_wt, ir=irtrain)) 
species.pred<-predict(ir.lda, ir)
table(ir$SPECIES, species.pred$class)

bf<-read.table("butterflies_2023.txt", header=TRUE)
head(bf)

co_matrx<-cor(bf[,2:8])
print(co_matrx)

(bfstd<-scale(bf[2:8]))
en_v<-bfstd[,1:4]
gen_v<-bfstd[,5:7]

cor(en_v,en_v)
cor(en_v,gen_v)
cor(gen_v,gen_v)

library(yacca)
bfstd.cca<-cca(en_v,gen_v)
summary(bfstd.cca)
F.test.cca(bfstd.cca)

