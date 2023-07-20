#load dataset
fl <-read.table("film_2023.txt", header=TRUE)
#structure of dataset
str(fl)
#add library
library(MVN)
#
dt<-fl[2:5]
mvn(dt, univariateTest = "sw", univariatePlot = "qqplot")
mvn(dt, univariatePlot = "histogram")

# Load the required package
library(scatterplot3d)
dt<-fl[2:3]
# Create a perspective plot
p_plot <- scatterplot3d(x = dt[,1], y = dt[,2], z = NULL, color = "red", pch = 16, angle = 30, type = "h", main = "Perspective Plot")


# Create a contour plot
library("MASS")
contour(x = seq(min(dt[,1]), max(dt[,1]), length = 100), y = seq(min(dt[,2]), max(dt[,2]), length = 100),
        z = kde2d(dt[,1], dt[,2], n = 100)$z, main = "Contour Plot")

mvnormtest::mshapiro.test(t(dt))

#load dataset
ir <-read.table("iris_2023.txt", header=TRUE)
#structure of dataset
str(ir)
summary(ir)


# Plot a scatterplot matrix of the four flower characteristic variables
pairs(ir[2:5], main = "Scatterplot Matrix of Iris Dataset",
      pch = 21, bg = c("red", "green3", "blue")[unclass(ir$Species)])

# Set the color palette for the three species
col <- c("red", "green3", "blue")

sub_ir<-ir[,c("SEPALLEN", "SEPALWID","PETALLEN","PETALWID")]
library(car)
scatterplotMatrix(x = sub_ir,diagonal = list(method = "density"), main = "Scatterplot Matrix of Iris Dataset")

# Add a legend
legend("bottomleft", legend = levels(ir$Species), pch = 1, col = col, bg = "white")

flower_manova<-manova(cbind(SEPALLEN, SEPALWID, PETALLEN, PETALWID)~ SPECIES, data=ir)
summary(flower_manova)
library(car)
summary(flower_manova,test = "Wilks")
summary(flower_manova, test="Pillai")
summary(flower_manova, test="Hotelling-Lawley")
summary(flower_manova, test="Roy")

library(Hotelling)


sep <- ir[ir$Species == "sepallen", 2:5]
sepld <- ir[ir$Species == "sepalwid", 2:5]
pet <- ir[ir$Species == "petallen", 2:5]

library(mvnormtest)


ht.sep.pet <- hotelling.test(sep[,2:5], pet[,2:5], alpha=0.05)
ht.sep.pet

# Compare setosa and virginica
ht.sepld.pet <- hotelling.test(sepld[,2:5],pet[,2:5], alpha=0.05)
ht.sepld.pet

# Compare versicolor and virginica
ht.sepld.sep <- hotelling.test(sepld[,2:5], sep[,2:5], alpha=0.05)
ht.sepld.sep

usair <- read.table("usair_2023.dat", header = TRUE)
str(usair)
summary(usair)
unpair_pca<-usair[,c("SO2","temp", "wind.speed","annual.precip","days.precip")]
pca_result<-prcomp(usair,scale. = TRUE)
print(pca_result)

pca<-prcomp(usair[,c("SO2","temp", "wind.speed","annual.precip","days.precip")],scale= TRUE)
e_values<-pca$sdev^2
p_variation<-e_values/sum(e_values)*100
c_p_va<-cumsum(p_variation)
plot(p_variation, type="b",xlab="P Comp.", ylab="% Vari.", main="Scree Plot")
lines(c_p_va, type="b", col="red")
legend("topright",c("%Vari.","Cum % varia"),lty=c(1,1), col = c("black","red"))

# Load the usair data
plot(pca, type = "l")
loadings <- pca$rotation[, 1:2]
# Print the loadings
print(loadings)
# Plot the variable loadings
biplot(pca, scale = 0)
# Calculate the correlation between PC1 and PC2
correlation <- cor(pca$x[, 1], pca$x[, 2])
# Print the correlation
print(correlation)

co_mat<-cor(usair[,c("SO2","temp", "wind.speed","annual.precip","days.precip")])
print(co_mat)
cov_mat<-cov(usair[,c("SO2","temp", "wind.speed","annual.precip","days.precip")])
print(cov_mat)

