# Self Organizing Mapping on MLB Data

## SOMs are an unsupervised data visualization technique that can be used to visualize high-dimensial data sets in lower (2-D) dimensional representations.
## We will include descriptions for the code used.

# The SOM Grid
## The Grid consists a map of multiple "nodes". Each node vector has a characteristic.
### - A fixed position on the SOM grid
### - A weight vector of the same dimension as the input space. 
###   (for example: if your input data represented people, it could have variables "age", "sex", "height", and others, each node on the grid will also have values for these variables)
### - Associated samples from the input data. Each sample in the input space is "mapped" to a node on the mpa grid. 
###   One node can represent several input samples (THIS IS IMPORTANT)

## Real World Example
### If everyone in the room stood up and compared attributes about themselves (height, age, gender, major, etc.).
### Everyone moves until they are closest to the people with similar attributes (Euclidean distance).
### Then raise a card indicating one of the attributes - that would be a SOM heatmap.

#Load the Libraries
library(kohonen)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(purrr)
library(dummy)
library(rgdal)

#Load in Data
MLB_2022_season <- read.csv("~/R-Master-Folder/data/MLB_2022_season.csv")
View(MLB_2022_season)

#Rename some variables to work easier
MLB_2022_season <- MLB_2022_season %>%
  rename(Walk_Rate = BB.,
         Strikeout_Rate = K.,
         Singles = X1B,
         Doubles = X2B,
         Triples = X3B,
         wRC_plus = wRC.)
str(MLB_2022_season)

#Color palette used for all the heatmaps
coolBlueHotRed <- function(n, alpha = 1){
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#Color palette for clustering
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

#Creating the SOM
require(kohonen)

MLB_train <- MLB_2022_season[, c(5, 6, 7, 8, 14, 15, 16, 17)] #Choosing columns of Singles, Doubles, Triples, Homers, BABIP, AVG, OBP, SLG

MLB_train_matrix <- as.matrix(scale(MLB_train)) #scale() centers the mean and normalizes all columns

MLB_som_grid <- somgrid(xdim = 5, ydim = 5, topo = "rectangular") #Creating a hexagonal SOM grid with 25 nodes (this will give us about 8 samples per node)
set.seed(2022) #replicating the model
MLB_som_model <- som(MLB_train_matrix,
                     grid = MLB_som_grid,
                     rlen = 100,
                     alpha = c(.05, .01),
                     keep.data = TRUE) #Defining the parameters of the model. Rlen: times to present data, Alpha: learning rate, keep.data: store data in the model.
summary(MLB_som_model)
plot(MLB_som_model, type = "count",
     main = "Node Counts") # We need to make sure the nodes don't have small counts. Need anywhere from 5-10.

plot(MLB_som_model, type = "codes",
     main = "Code Counts") # This shows us all the variables and how they relate to each other.

# Plotting a Heatmap SOM
## A heatmap shows the distribution of a variable across the SOM.
## An example: If everyone in the room raised a colored card according to their age - that would be a heatmap.
## The idea is that the SOM trains the data to aggregate people of similar characteristic in that one particular variabel together
## We can also explore the relationship between variables.

# Normalized heatmap
plot(MLB_som_model, type = "property", property = getCodes(MLB_som_model)[,1], main=colnames(getCodes(MLB_som_model))[1], palette.name=coolBlueHotRed)

# Unscaled Heatmaps
var <- 1 #define the variable to plot
var_unscaled <- aggregate(as.numeric(MLB_train[,var]), by=list(MLB_som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(MLB_som_model, type = "property", property=var_unscaled, main=names(MLB_train)[var], palette.name=coolBlueHotRed)

# Heatmaps descriptions
## Heatmaps are the most important visualization possible for SOMs.
## A Heatmap allows the visualization of the distribution of a single variable across the map.
## We can use it to compare across variables. It is important to remember that the individual sample positions do not move from one visualization to another,
## the map simply changes color by different variable.

# Compare all the averages to see the relationship
var <- 5 #define the variable to plot
var_unscaled <- aggregate(as.numeric(MLB_train[,var]), by=list(MLB_som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(MLB_som_model, type = "property", property=var_unscaled, main=names(MLB_train)[var], palette.name=coolBlueHotRed)

var <- 6 #define the variable to plot
var_unscaled <- aggregate(as.numeric(MLB_train[,var]), by=list(MLB_som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(MLB_som_model, type = "property", property=var_unscaled, main=names(MLB_train)[var], palette.name=coolBlueHotRed)

var <- 7 #define the variable to plot
var_unscaled <- aggregate(as.numeric(MLB_train[,var]), by=list(MLB_som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(MLB_som_model, type = "property", property=var_unscaled, main=names(MLB_train)[var], palette.name=coolBlueHotRed)

var <- 8 #define the variable to plot
var_unscaled <- aggregate(as.numeric(MLB_train[,var]), by=list(MLB_som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(MLB_som_model, type = "property", property=var_unscaled, main=names(MLB_train)[var], palette.name=coolBlueHotRed)

# Clustering the SOM

## showing the WCSS metric for kmeans for different clustering sizes
MLB_data <- getCodes(MLB_som_model)
MLB_wss <- (nrow(MLB_data)-1)*sum(apply(MLB_data, 2, var))
for (i in 2:15) {MLB_wss[i] <- sum(kmeans(MLB_data, centers = i)$withinss)}

par(mar =c(5.1,4.1,4.1,2.1))
plot(1:15, MLB_wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

## form clusters on the grid
MLB_som_clusters <- cutree(hclust(dist(getCodes(MLB_som_model))), 5)

## show map with different colors for clusters
plot(MLB_som_model, type="mapping", bgcol = pretty_palette[MLB_som_clusters], main = "Clusters")
add.cluster.boundaries(MLB_som_model, MLB_som_clusters)

## show the same plot with the codes
plot(MLB_som_model, type="codes", bgcol = pretty_palette[MLB_som_clusters], main = "Clusters")
add.cluster.boundaries(MLB_som_model, MLB_som_clusters)

# Assign a new column telling where each player is clustered.
# get vector with cluster value for each original data sample
cluster_assignment <- MLB_som_clusters[MLB_som_model$unit.classif]
# for each of analysis, add the assignment as a column in the original data:
MLB_2022_season$cluster <- cluster_assignment

# Predict the data
MLB_train1 <- MLB_2022_season[, c(5, 6, 7, 8, 14, 15, 16, 17, 22)]
set.seed(6)
indicies <- sample(2, nrow(MLB_train1), replace = T, prob = c(.7, .3))
further_train <- MLB_train1[indicies == 1,]
further_test <- MLB_train1[indicies == 2,]

MLB_supervised_trainX <- scale(further_train[,-9])
MLB_supervised_testX <- scale(further_test[,-9],
                              center = attr(MLB_supervised_trainX, "scaled:center"),
                              scale = attr(MLB_supervised_trainX, "scaled:scale"))
MLB_supervised_trainY <- factor(further_train[,9])
MLB_supervised_Y <- factor(further_test[,9])
further_test[,9] <- 0
further_testXY <- list(independent = MLB_supervised_testX, dependent = further_test[,9])

# Gradient boosting
set.seed(17)
SOM_map1 <- xyf(MLB_supervised_trainX,
            classvec2classmat(factor(MLB_supervised_trainY)),
            grid = somgrid(5, 5, "rectangular"),
            rlen = 100)
plot(SOM_map1, type='codes',palette.name = coolBlueHotRed)

# Cluster Boundaries
par(mfrow = c(1,2))
plot(SOM_map1,
     type = 'codes',
     main = c("Codes X", "Codes Y"))
map1.hc <- cutree(hclust(dist(SOM_map1$codes[[2]])), 5)
add.cluster.boundaries(SOM_map1, map1.hc)
par(mfrow = c(1,1))

# prediction model (it sucks)
pred <- predict(SOM_map1, newdata = further_testXY)
table(Predicted = pred$predictions[[2]], Actual = MLB_supervised_Y)
