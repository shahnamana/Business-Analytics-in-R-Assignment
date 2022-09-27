library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

setwd("E:/Khushi/UTD/BawithR/Datasets")
pharma.df <- read.csv("Pharmaceuticals.csv")

#--------------------2a------------------------
row.names(pharma.df) <- pharma.df[,1]

#Remove the symbol column
pharma.df <- pharma.df[,-1]

#Remove the Name column
pharma.df <- pharma.df[,-1]

#Remove the Median_Recommendation column
pharma.df <- pharma.df[,-10]

#Remove the Location column
pharma.df <- pharma.df[,-10]

#Remove the Exchange column
pharma.df <- pharma.df[,-10]

#--------------------2b------------------------

#Normalize input variables
pharma.df.norm <- sapply(pharma.df, scale)

#Add row names: Symbols
row.names(pharma.df.norm) <- row.names(pharma.df)

#--------------------2c------------------------

#Compute normalized distance based on all 9 variables
d.norm <- dist(pharma.df.norm, method = "euclidean")

#In hclust(), set argument method = to "ward.D", "single", "complete", "average", "median", or 
"centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = TRUE)

#--------------------2d------------------------

memb <- cutree(hc1, k = 6)
memb

#Using hclust to show better grouping; dendexten
plot(hc1, cex = 1)
rect.hclust(hc1, k = 6, border = 2:5)

#--------------------2e------------------------

hc2 <- hclust(d.norm, method = "complete")
plot(hc2, hang = -1, ann = TRUE)

#--------------------2f------------------------

memb <- cutree(hc2, k = 6)
memb

#Using hclust to show better grouping; dendexten
plot(hc2, cex = 1)
rect.hclust(hc2, k = 6, border = 2:5)

#--------------------2g------------------------

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
library(dendextend)
tanglegram(dend1, dend2)
