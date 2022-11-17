# BUAN 448 Week 12 Chapter 15 part 2

# =============== Public Utilities Example ==============
library(ggplot2)
library(ggrepel)
utilities.df <- read.csv("Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

# remove the utility column
utilities.df <- utilities.df[,-1]

#Plot the Fuel_cost over Sales 
ggplot(utilities.df,aes(x=Sales,y=Fuel_Cost,label=row.names(utilities.df))) + 
  geom_point() + geom_text_repel() + labs(y="Fuel Cost")

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df, method = "euclidean")



# =============== Normalizing and Finding Distance  ==============

# normalize input variables 
# First Approach is to use sapply function
utilities.df.norm <- sapply(utilities.df, scale)
# Second Approach is to use preProcess function 
library(caret)
norm.values <- preProcess(utilities.df, method=c("center","scale"))
utilities.df.norm2 <- predict(norm.values, utilities.df)

# compute normalized distance based on all 8 variables
d.norm.all <- dist(utilities.df.norm, method = "euclidean")

# =============== Hierarchical Clustering  ==============

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"

hc3 <- hclust(d.norm.all, method = "single")
plot(hc3, hang = -1, cex = 1, ann = FALSE)
library(ggplot2)
library(ggdendro)
ggdendrogram(hc3, theme_dendro = FALSE)+labs(x=NULL,y=NULL)
#  4 units distance
memb3 <- cutree(hc3, h=3)
# Showing members of each clusters
split(names(memb3), memb3)

hc4 <- hclust(d.norm.all, method = "average")
plot(hc4, hang = -1, ann = FALSE)
ggdendrogram(hc4, theme_dendro = FALSE)+labs(x=NULL,y=NULL)

memb4 <- cutree(hc4, h=3)
split(names(memb4), memb4)

hc.ward <- hclust(d.norm.all, method = "ward.D")
plot(hc.ward, hang = -1, ann = FALSE)
ggdendrogram(hc.ward, theme_dendro = FALSE)+labs(x=NULL,y=NULL)

memb.ward <- cutree(hc.ward, k=4)
split(names(memb.ward), memb.ward)

# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb.ward, ": ", row.names(utilities.df), sep = "")
row.names(utilities.df.norm) <- paste(memb4, ": ", row.names(utilities.df), sep = "")


# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

# ====================== K-Means Clustering Algorithm ===============
# Tiny Example 
df <- data.frame(x=c(1,1.5,3,5,3.5,4.5,3.5),y=c(1,2,4,7,5,5,4.5))
row.names(df) <- c("one","two","three","four","five","six","seven")
library(ggplot2)

ggplot(df,aes(x=x,y=y)) + geom_point() + labs(x=NULL,y=NULL)

# normalized distance:
df.norm <- sapply(df, scale)
row.names(df.norm) <- row.names(df) 

# run kmeans algorithm 
set.seed(2)
km <- kmeans(df.norm, 2)

# show cluster membership
km$cluster
# The coordinates of clusters' centroid
km$centers

library(factoextra)
fviz_cluster(km, df.norm, main = "Tiny Example Cluster plot"))
fviz_cluster(km, df.norm, ellipse.type = "norm",ellipse.level = .7, main = "Tiny Example Cluster plot")
fviz_cluster(km, df.norm, geom = "point",main = "Example Cluster plot")
fviz_cluster(km, df.norm, geom = "text",main = "Example Cluster plot")
fviz_cluster(km, df.norm, ellipse.type = "convex",main = "Example Cluster plot")
fviz_cluster(object = km, # kmeans object 
             data = df.norm, # data used for clustering
             ellipse.type = "norm",
             ellipse.level = 0.7, # the size of the concentration ellipse in normal probability. Default is 0.95 
             geom = "point",
             palette = "jco", # the color is used to show cluster
             main = "Cluster plot",
             ggtheme = theme_minimal())

# Back to the Utility Example
# Since we have done some modification on the original dataset
# load and preprocess data 
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# run kmeans algorithm 
set.seed(1234)
km4.ut <- kmeans(utilities.df.norm, 4)

# show cluster membership
km4.ut$cluster
fviz_cluster(km4.ut, utilities.df.norm, geom = "point",main = "Cluster plotfor Utilities Firms")
fviz_cluster(km4.ut, utilities.df.norm, main = "Cluster plot for Utilities Firms",choose.vars = c("Sales", "RoR"))
#  if you omit the choose.vars argument, the function fviz_cluster transforms the initial set of variables into a new set of variables through principal component analysis (PCA). The algorithm outputs two new variables (Dim1 and Dim2) that represent the original variables, a projection or "shadow" of the original data set.
# Each dimension represent a certain amount of the variation (i.e. information) contained in the original data set.
fviz_cluster(object = km4.ut, # kmeans object 
             data = utilities.df.norm, # data used for clustering
             ellipse.type = "norm",
             ellipse.level = 0.85, # the size of the concentration ellipse. 
             geom = "point",
             palette = "jco", # the color is used to show cluster
             main = "Cluster plot for Utilities Firms",
             ggtheme = theme_grey()) 
# theme_classic() theme_grey() theme_dark() theme_void() theme_minimal()

fviz_cluster(object = km4.ut, # kmeans object 
             data = utilities.df.norm, # data used for clustering
             geom = "point",
             palette = "jco", # the color is used to show cluster
             main = "Cluster plot for Utilities Firms",
             ggtheme = theme_grey())

fviz_cluster(km4.ut, utilities.df.norm)
# this set.seed() is not necessary here, I just placed it for similarity with the case of 4 clusters
set.seed(1234)
km5.ut <- kmeans(utilities.df.norm, 5)
fviz_cluster(km5.ut, utilities.df.norm, main = "Cluster plot for Utilities Firms")
# this set.seed() is not necessary here, I just placed it for similarity with the case of 4 clusters
set.seed(1234)
km6.ut <- kmeans(utilities.df.norm, 6)
fviz_cluster(km6.ut, utilities.df.norm, main = "Cluster plot for Utilities Firms")

library(gridExtra)
p4 <- fviz_cluster(km4.ut, utilities.df.norm, main = "Four clusters")
p5 <- fviz_cluster(km5.ut, utilities.df.norm, main = "Five clusters")
p6 <- fviz_cluster(km6.ut, utilities.df.norm, main = "Six clusters")
grid.arrange(p4, p5, p6, nrow = 1)

# examine the cluster centroids
km6.ut$centers
# find the sum squared distances within clusters
km6.ut$withinss
# find the clusters' sizes 
km6.ut$size
# find the clusters' distances 
dist(km6.ut$centers)

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km6.ut$centers), max(km6.ut$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6)){
  lines(km6.ut$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "gray70"))
}
# name clusters
text(x = 0.3, y = km6.ut$centers[, 1], labels = paste("Cluster", c(1:6)))
text(x = 8.1, y = km6.ut$centers[, 8], labels = paste(c(1:6)))


# Determining Optimal Clusters with Elbow Method
fviz_nbclust(utilities.df.norm, kmeans, method = "wss")

# Determining Optimal Clusters with Average Silhouette Method
fviz_nbclust(utilities.df.norm, kmeans, method = "silhouette")

