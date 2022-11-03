# BUAN 448 
## Week 10: performance of prediction models

rm(list=ls(all=TRUE))
cat("\014")
# load file
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
# randomly generate training and validation sets
set.seed(2)
training.index <- sample(toyota.corolla.df$Id, 862)
train.df <- toyota.corolla.df[training.index, ]
valid.df <- toyota.corolla.df[-training.index, ]
t(t(names(train.df)))

# run linear regression model
reg <- lm(Price~., data=train.df[,-c(1,2,8,11)],
          na.action=na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata=valid.df[,-c(1,2,8,11)],
                  na.action=na.pass)
# package forecast is required to evaluate performance
library(forecast)
## evaluate performance
# training
a.train <- accuracy(pred_t, train.df$Price)
# validation
b.valid <- accuracy(pred_v, valid.df$Price)
row.names(a.train) <- "train set"
row.names(b.valid) <- "valid set"
options(scipen = 100, digits = 6)
c <- rbind(a.train,b.valid)
c

model.result.train <- data.frame("Predicted" = pred_t, "Actual" = train.df$Price, "residual" = (train.df$Price - pred_t))

model.result.valid <- data.frame("Predicted" = pred_v, "Actual" = valid.df$Price, "residual" = (valid.df$Price - pred_v))

# BoxPlots for residuals 
library(ggplot2) # load for ggplot
library(gridExtra) # for for grid plots

p1 <- ggplot(data = model.result.train, aes( x ="Training Set" ,y = residual)) +
  geom_boxplot()  + theme(axis.title.x=element_blank())
p2 <- ggplot(data = model.result.valid, aes(x = "Validation Set", y = residual)) + 
  geom_boxplot() +  theme(axis.title.x=element_blank())
grid.arrange(p1, p2, nrow = 1)


# histogram for residuals
p3 <- ggplot(data = model.result.train, aes(x = residual)) +
  geom_histogram() + labs(x = "Training Set")

p4 <- ggplot(data = model.result.valid, aes(x = residual)) + 
  geom_histogram() + labs(x = "Validation Set") 
grid.arrange(p3, p4, nrow = 1)



# ========================
car.df <- read.csv("ToyotaCorolla.csv")
#selected variables for regression
t(t(names(train.df)))

selected.df <- car.df[c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)]

#partition data
set.seed(12345) #set seed for reproducing the partition
train.index <- sample(c(1:1436), 862)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]



##comparing models:
car.lm.1 <- lm(Price ~ Age_08_04, data = train.df)
car.lm.2 <- lm(Price ~ Age_08_04 + Weight, data = train.df)
car.lm.3 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type, data = train.df)
car.lm.4 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM, data = train.df)
car.lm.5 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM + Quarterly_Tax, data = train.df)
car.lm.6 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM + Quarterly_Tax + Doors, data = train.df)
car.lm.7 <- lm(Price ~ Age_08_04 + Weight + Fuel_Type +KM + Quarterly_Tax + Doors + HP, data = train.df)

car.lm.1.valid.pred <- predict(car.lm.1, valid.df)
car.lm.2.valid.pred <- predict(car.lm.2, valid.df)
car.lm.3.valid.pred <- predict(car.lm.3, valid.df)
car.lm.4.valid.pred <- predict(car.lm.4, valid.df)
car.lm.5.valid.pred <- predict(car.lm.5, valid.df)
car.lm.6.valid.pred <- predict(car.lm.6, valid.df)
car.lm.7.valid.pred <- predict(car.lm.7, valid.df)

car.lm.1.train.pred <- predict(car.lm.1, train.df)
car.lm.2.train.pred <- predict(car.lm.2, train.df)
car.lm.3.train.pred <- predict(car.lm.3, train.df)
car.lm.4.train.pred <- predict(car.lm.4, train.df)
car.lm.5.train.pred <- predict(car.lm.5, train.df)
car.lm.6.train.pred <- predict(car.lm.6, train.df)
car.lm.7.train.pred <- predict(car.lm.7, train.df)

library(forecast)
car.lm.1.train.accuracy <- accuracy(car.lm.1.train.pred, train.df$Price)
car.lm.2.train.accuracy <- accuracy(car.lm.2.train.pred, train.df$Price)
car.lm.3.train.accuracy <- accuracy(car.lm.3.train.pred, train.df$Price)
car.lm.4.train.accuracy <- accuracy(car.lm.4.train.pred, train.df$Price)
car.lm.5.train.accuracy <- accuracy(car.lm.5.train.pred, train.df$Price)
car.lm.6.train.accuracy <- accuracy(car.lm.6.train.pred, train.df$Price)
car.lm.7.train.accuracy <- accuracy(car.lm.7.train.pred, train.df$Price)

car.lm.1.valid.accuracy <- accuracy(car.lm.1.valid.pred, valid.df$Price)
car.lm.2.valid.accuracy <- accuracy(car.lm.2.valid.pred, valid.df$Price)
car.lm.3.valid.accuracy <- accuracy(car.lm.3.valid.pred, valid.df$Price)
car.lm.4.valid.accuracy <- accuracy(car.lm.4.valid.pred, valid.df$Price)
car.lm.5.valid.accuracy <- accuracy(car.lm.5.valid.pred, valid.df$Price)
car.lm.6.valid.accuracy <- accuracy(car.lm.6.valid.pred, valid.df$Price)
car.lm.7.valid.accuracy <- accuracy(car.lm.7.valid.pred, valid.df$Price)

train.valid.rmse.df <- data.frame("Model#" = c(1,2,3,4,5,6,7), 
                                  "Valid_RMSE" = c(car.lm.1.valid.accuracy[1,2],
                                                   car.lm.2.valid.accuracy[1,2],
                                                   car.lm.3.valid.accuracy[1,2],
                                                   car.lm.4.valid.accuracy[1,2],
                                                   car.lm.5.valid.accuracy[1,2],
                                                   car.lm.6.valid.accuracy[1,2],
                                                   car.lm.7.valid.accuracy[1,2]),
                                  "Train_RMSE" = c(car.lm.1.train.accuracy[1,2],
                                                   car.lm.2.train.accuracy[1,2],
                                                   car.lm.3.train.accuracy[1,2],
                                                   car.lm.4.train.accuracy[1,2],
                                                   car.lm.5.train.accuracy[1,2],
                                                   car.lm.6.train.accuracy[1,2],
                                                   car.lm.7.train.accuracy[1,2]))


train.valid.rmse.df$Train_RMSE %>% plot(type = "o",col = "red", xlab = "Model#", ylab = "RMSE", 
     main = "Train vs. Valid Performance, RSME")
lines(train.valid.rmse.df$Valid_RMSE, type = "o", col = "blue") 
legend("topright", legend=c("training", "validation"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

# ====================================
# performance of classification models
# recall the practice assignment 
rm(list=ls())
cat("\014")

RM.df <- read.csv("RidingMowers.csv")
str(RM.df)

#use one of the following way to convert Ownership column work in glm function
#Method 1
RM.df$Ownership[RM.df$Ownership == "Owner"] <- 1
RM.df$Ownership[RM.df$Ownership == "Nonowner"] <- 0
RM.df$Ownership <-as.numeric(RM.df$Ownership)

# Method 2
RM.df$Ownership <- as.factor(RM.df$Ownership)
str(RM.df)



library(caret)
# logistic regression
logit.reg <- glm(Ownership ~ Income + Lot_Size , data = RM.df, family = "binomial")
options(scipen=999)
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, RM.df[ , -3], type = "response")
RM.df2 <- data.frame(actual = RM.df$Ownership, predicted = round(logit.reg.pred,2))

# Cutoff = 0.5
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(RM.df$Ownership))

# Cutoff = 0.25
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.25, 1, 0)), as.factor(RM.df$Ownership))

# Cutoff = 0.75
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.75, 1, 0)), as.factor(RM.df$Ownership))


# looking for optimal cutoff
cmatrix0.1 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.1, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.2 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.2, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.3 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.3, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.4 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.4, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.5 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.6 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.6, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.7 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.7, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.8 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.8, 1, 0)), as.factor(RM.df$Ownership))$overall[1]
cmatrix0.9 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.9, 1, 0)), as.factor(RM.df$Ownership))$overall[1]

cutoff <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cmatraix.all <- c(cmatrix0.1, cmatrix0.2, cmatrix0.3, cmatrix0.4, cmatrix0.5, cmatrix0.6,
                  cmatrix0.7, cmatrix0.8, cmatrix0.9)
plot(cmatraix.all ~ cutoff,
     xlab="Cutoff Values", ylab="Accuracy", main="", type="l", ylim = c(0,1))

lines(1-cmatraix.all ~ cutoff,lty="dashed",col="green")
legend("topright", legend=c("accuracy", "Overall Error"),
       col=c("black", "green"), lty=1:2, cex=0.8)

############## Find the best cut-off value using a for loop
acc.df <- NULL

for (i in seq(0.1,.99,0.01)){
  print(i)
  cmatrixi <- confusionMatrix(as.factor(RM.df$Ownership), as.factor(ifelse(logit.reg.pred > i, 1, 0)))$overall[[1]]
  acc.df <- rbind(acc.df,data.frame(cutoff=i,cmatrix=cmatrixi))
}
library(ggplot2)
p <- ggplot(data = acc.df, mapping = aes(x =cutoff, y=cmatrix))
p + geom_point() + ylim(0,1)+
  geom_line()+
  labs(x = "Cutoff",
       y = "Accuracy",
       title = "Model Prediction Accuray using Different Cutoff Values")
# acc.df
# accT = c()
# for(cutoff in seq(0.1, 0.9, 0.1)){
#   cm <- confusionMatrix(as.factor(ifelse(logit.reg.pred > cutoff, 1, 0)), as.factor(RM.df$Ownership))
#   accT = c(accT, cm$overall[1])
# }

# Plot accuracy
# plot(accT ~ seq(0.1, 0.9, 0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0,1))
# Plot error
# lines(1-accT ~ seq(0.1,0.9,0.1), type = "l", lty = 2)
# legend("topright", c("accuracy", "overall error"), lty = c(1,2), merge = T)


### ============= plot ROC curve ==========================================
# install.packages("pROC")
library(pROC)
r <- roc(RM.df$Ownership, logit.reg.pred)
#finding the best cutoff point
coords(r, "best")
#plotting roc
plot.roc(r)
plot(r, print.thres="best")

#compute area under the curve
auc(r)


# ===========   lift charts =====================================
# first option with 'gains' library:
library(gains)

gain <- gains(RM.df$Ownership, logit.reg.pred, groups=10)
gain 

plot(c(0,gain$cume.obs), c(0, gain$cume.pct.of.total*sum(RM.df$Ownership)),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(RM.df$Ownership))~c(0, dim(RM.df)[1]), lty=2)

#compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(RM.df$Ownership)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,2), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# Second option with 'caret' library:
library(caret)
lift.example <- lift(relevel(as.factor(actual), ref="1") ~ predicted, data = RM.df2)
xyplot(lift.example, plot = "gain")