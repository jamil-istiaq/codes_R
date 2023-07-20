#import data and change missing values to NA strings
river_data <- read.csv("river_2023.csv", na.strings = "XXXXXXX")

#remove missing values
river_clean<- na.omit(river_data) 

#view structure of clean data
str(river_clean) 

#calculate the missing cases
removed_cases <- nrow(river_data) - nrow(river_clean)    
#print number of missing cases
print(removed_cases)   
#season frequency calculation
season_freq <- table(river_clean$Season)
#river frequency calculation
river_size_freq <- table(river_clean$River_Size)
#print the frequencies
river_size_freq
season_freq
#summary of chemical variables
chem_summary <- summary(river_clean[,4:9])
#print summary
chem_summary
#Summary of algae variables
algae_summary <- summary(river_clean[,10:14])
#print summary
algae_summary
#boxplot of both chemical and algae variables
boxplot(river_clean[,4:9],main="Boxplot of Chem.Variables", las=2)
boxplot(river_clean[,10:14],main="Boxplot of Algae Variables", las=2)
  
# Create the 'river_size_vel' variable
river_clean$river_size_vel<-paste(river_clean$River_Size,river_clean$Fluid_vel)
# Frequency table of the number of rivers in each new category
table(river_clean$river_size_vel)
#calculating chemical vules with river size variable
j1 = tapply(river_clean$nitrogen,river_clean$river_size_vel,mean)
j2 = tapply(river_clean$nitrates,river_clean$river_size_vel,mean)
j3 = tapply(river_clean$nitrites,river_clean$river_size_vel,mean)
j4 = tapply(river_clean$ammonia,river_clean$river_size_vel,mean)
j5 = tapply(river_clean$phosphate,river_clean$river_size_vel,mean)
j6 = tapply(river_clean$oxygen,river_clean$river_size_vel,mean)
#creating data frame from the mean values
r_mean = data.frame(j1,j2,j3,j4,j5,j6)
r_mean
#clean any NA values
r_mean_clean<- na.omit(r_mean)
#plot dendrogram
hcluster <- hclust(dist(scale(r_mean_clean[1:6])), method = "complete")
hcluster
plot(hcluster , hang = -1 , cex=0.5)

# Perform MDS
dis<-dist(r_mean_clean[1:6])
mds<-cmdscale(dis, k = 2)
plot(mds[, 1], mds[, 2], pch = 7, col = "blue", xlab = "MDS1", ylab = "MDS2")
text(mds[, 1], mds[, 2], labels = hcluster$labels, pos = 3, cex = 0.7, col = "black")

# Compute the distance matrix using an appropriate distance metric (e.g., Euclidean, Manhattan, etc.)
dist_matrix <- dist(r_mean_clean[, 1:6], method = "euclidean")
# Perform hierarchical clustering using an appropriate clustering method (e.g., complete, average, etc.)
hc <- hclust(dist_matrix, method = "complete")
# Plot the dendrogram
plot(hc, hang =-1, lables = river_clean$Season, main = "Dendrogram of Seasons based on
Algae Variables")

#calculating algae values with seasons variable
i1 = tapply(river_clean$A1,river_clean$Season,mean)
i2 = tapply(river_clean$A2,river_clean$Season,mean)
i3 = tapply(river_clean$A3,river_clean$Season,mean)
i4 = tapply(river_clean$A4,river_clean$Season,mean)
#creating data frame from the mean values
a_mean = data.frame(i1,i2,i3,i4)
a_mean
a_mean_clean<- na.omit(a_mean)
#ploting dendrogram
hcluster <- hclust(dist(scale(a_mean_clean[1:4])), method = "complete")
hcluster
plot(hcluster , hang = -1 , cex=0.5)

# Step 1: Calculate mean frequencies for each algae variable by season
algae_vars <- c("A1", "A2", "A3", "A4", "A5")
mean_frequencies <- aggregate(river_clean[, algae_vars], by = list(river_clean$Season), FUN = mean)
#Step 2: Rename the columns in the mean_frequencies table
colnames(mean_frequencies) <- c("Season", algae_vars)
mean_frequencies
