#### Table 11.2

library(neuralnet) # for neuralnet function 

df <- read.csv("TinyData.csv")
View(df)

df$Like <- ifelse(df$Acceptance=="like",1,0) 
df$Dislike <- ifelse(df$Acceptance=="dislike",1,0)

set.seed(1)
nn <- neuralnet(Like + Dislike ~ Salt + Fat, data = df, linear.output = F, hidden = 3)

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")



#### Table 11.3

library(caret)
predict.taste <- predict(nn, data.frame(df$Salt, df$Fat))
predicted.class <- apply(predict.taste,1,which.max)-1
confusionMatrix(as.factor(ifelse(predicted.class=="1", "dislike", "like")), as.factor(df$Acceptance))

# Example for apply function 
m1 <- matrix(c(1,12,13,11,10,1,3,15,2), nrow = 3, ncol = 3)
m1
apply(m1, 1, sum) #Function based on row 
apply(m1, 2, sum) #Function based on column
apply(m1, 1, which.max) #Function based on which row is maximum
apply(m1, 2, which.max) #Function based on which row is maximum




#### Table 11.6, 11.7
library(neuralnet) # for neuralnet function 
library(nnet) # for class.ind function 
library(caret) # for confusionMatrix function 

accidents.df <- read.csv("accidentsnn.csv")
# selected variables
vars <- c("ALCHL_I", "PROFIL_I_R", "VEH_INVL")

# partition the data
set.seed(20)
training=sample(row.names(accidents.df), dim(accidents.df)[1]*0.6)
validation=setdiff(row.names(accidents.df), training)

# when y has multiple classes - need to dummify
trainData <- cbind(accidents.df[training,c(vars)], 
                   class.ind(accidents.df[training,]$SUR_COND),
                   class.ind(accidents.df[training,]$MAX_SEV_IR))
names(trainData) <- c(vars, 
                   paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))

validData <- cbind(accidents.df[validation,c(vars)], 
                   class.ind(accidents.df[validation,]$SUR_COND),
                   class.ind(accidents.df[validation,]$MAX_SEV_IR))
names(validData) <- c(vars, 
                   paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))

# run nn with 2 hidden nodes 
# use hidden= with a vector of integers specifying number of hidden nodes in each layer
nn <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~ 
                  ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 + SUR_COND_2 
                + SUR_COND_3 + SUR_COND_4, data = trainData, hidden = 2)

# training.prediction <- compute(nn, trainData[,-c(8:11)])
training.prediction <- predict(nn, trainData[,-c(8:11)])

training.class <- apply(training.prediction,1,which.max)-1
confusionMatrix(as.factor(training.class), as.factor(accidents.df[training,]$MAX_SEV_IR))

validation.prediction <- predict(nn, validData[,-c(8:11)])
validation.class <- apply(validation.prediction,1,which.max)-1
confusionMatrix(as.factor(validation.class), as.factor(accidents.df[validation,]$MAX_SEV_IR))
