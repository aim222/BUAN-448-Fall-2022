# BUAN 448 Week 12 Chapter 15 part 1

# =============== Public Utilities Example ==============
library(ggplot2)
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
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")



# =============== Normalizing and Finding Distance  ==============

# normalize input variables 
# First Approach is to use sapply function
utilities.df.norm <- sapply(utilities.df, scale)
# Second Approach is to use preProcess function 
library(caret)
norm.values <- preProcess(utilities.df, method=c("center","scale"))
utilities.df.norm2 <- predict(norm.values, utilities.df)

# Just check the two methods 
s <- utilities.df.norm-utilities.df.norm2


# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")
options(digits=2)
d.norm 


# =============== Hierarchical Clustering  ==============

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"

hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
# The fraction of the plot height by which labels should hang below the rest of the plot. A negative value will cause the labels to hang down from 0.


hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)

# Dividing 3 clusters
memb <- cutree(hc1, k=3)
# Showing members of each clusters
split(names(memb), memb)

# Dividing based on the distance
memb <- cutree(hc1, h=.9)
# Showing members of each clusters
split(names(memb), memb)
# Showing members of each clusters in another way
split(row.names(utilities.df.norm), memb)

memb2 <- cutree(hc2, k = 3)
split(names(memb2), memb2)

# If distance of 0.65 , data can be reduced to 5 clusters in Single Linkage
mem6 <- cutree(hc1, h=.65)
split(names(mem6), mem6)

# If distance of 2.65 , data can be reduced to 5 clusters in Average Linkage
mem7 <- cutree(hc2, h=1.5)
split(names(mem7), mem7)


# compute normalized distance based on all 8 variables
d.norm.all <- dist(utilities.df.norm, method = "euclidean")

hc3 <- hclust(d.norm.all, method = "single")
plot(hc3, hang = -1, ann = FALSE)


# Dividing 3 clusters
memb3 <- cutree(hc3, k=3)
# Showing members of each clusters
split(names(memb3), memb3)

hc4 <- hclust(d.norm.all, method = "average")
plot(hc4, hang = -1, ann = FALSE)

# Dividing 3 clusters
memb4 <- cutree(hc4, k=3)
# Showing members of each clusters
split(names(memb4), memb4)
