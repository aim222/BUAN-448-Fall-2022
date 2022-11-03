# BUAN 448 Week 11


rm(list=ls())

mower.df <- read.csv("RidingMowers.csv")
str(mower.df)


# =====================#step 1 Partition data ===================
set.seed(12344)

train.index <- sample(c(1:dim(mower.df)[1]), dim(mower.df)[1]*0.6)
train.df <- mower.df[train.index, ]
valid.df <- mower.df[-train.index, ]

#scatterplot
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
?pch
text(train.df$Income, train.df$Lot_Size, pos = 3, rownames(train.df))
text(60, 20, "X",pch = c(1,3,4))
legend("bottomright", c("owner", "non-owner", "newhoursehold"), pch = c(1, 3, 4))

# =========================#step 2 normalize the data ==================
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

library(caret)
?preProcess
norm.values <- preProcess(train.df[, 1:2], method=c("center","scale"),rangeBounds = c(0,1))
train.norm.df[,1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[,1:2] <- predict(norm.values, valid.df[, 1:2])
mower.norm.df[,1:2] <- predict(norm.values, mower.df[, 1:2])

## =============   Example: new household ===============
new.df <- data.frame(Income = 60, Lot_Size = 20)
new.norm.df <- predict(norm.values, new.df)


# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable).
if (!require("FNN")) install.packages("FNN")
library(FNN)
# k = 1
nn1 <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 1)
nn1
#find the nearest nighbors
row.names(train.df)[attr(nn1, "nn.index")]
# k = 3
nn3 <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 3)
nn3 
row.names(train.df)[attr(nn3, "nn.index")]

# k = 5
nn5 <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 5)
nn5
row.names(train.df)[attr(nn5, "nn.index")]


# ====== step 3 Predict the entire validation set with a prespecified k ======

mower.knn.pred <- knn(train.norm.df[, 1:2], test = valid.norm.df[, 1:2], cl = train.norm.df[, 3], k=3)
mower.knn.pred
confusionMatrix(as.factor(mower.knn.pred), as.factor(valid.norm.df[, 3]))

# =================== step 4 Find the best K ===============
# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
accuracy.df
# compute knn for different k on validation.
for(i in c(1:14)) {
  mower.knn.pred <- knn(train.norm.df[, 1:2], test = valid.norm.df[, 1:2], cl = train.norm.df[, 3], k=i)
  accuracy.df[i, 2] <- confusionMatrix(mower.knn.pred, as.factor(valid.norm.df[, 3]))$overall[1]
}
accuracy.df
koptimal <- which(accuracy.df[,2] == max(accuracy.df[,2]))
koptimal
#  ===== step 5 classify new household after we know optimal k ======== 

plot(Lot_Size ~ Income, data=mower.df, pch=ifelse(mower.df$Ownership=="Owner", 1, 3))
text(mower.df$Income, mower.df$Lot_Size, pos = 3, rownames(mower.df))
text(60, 20, "X",pch = c(1,3,4))
legend("bottomright", c("owner", "non-owner", "newhoursehold"), pch = c(1, 3, 4))


new.df <- data.frame(Income = 60, Lot_Size = 20)
new.df

scoremower.norm.df <- predict(norm.values, new.df)
mower.knn.new <- knn(mower.norm.df[, 1:2], scoremower.norm.df, cl = mower.norm.df[, 3], k = koptimal[1])
mower.knn.new
row.names(mower.norm.df)[attr(mower.knn.new, "nn.index")]