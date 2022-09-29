# Practice
rm(list=ls(all=TRUE))
cat("\014")
#load the data and preprocess
housing.df <- read.csv("BostonHousing.csv")
head(housing.df)
t(t(names(housing.df)))
#remove the categorical response variable CAT..MEDV
housing.df <- housing.df[,-c(14)]
t(t(names(housing.df)))
## a) Fit a multiple linear regression model to the median house price 
##(MEDV) as a function of CRIM, CHAS, and RM. Write the equation for predicting 
##the median house price from the predictors in the model.

# fit the model
reg <- lm(MEDV ~ CRIM + CHAS + RM, data = housing.df)
summary(reg)

# b) Regression equation is
#MEDV = -28.81068 + (-0.26072 * CRIM) + (3.76304 * CHAS) + (8.27818 * RM)

## c) Using the estimated regression model, what median house price is 
##predicted for a tract in the Boston area that does not bound the Charles 
##River, has a crime rate of 0.1, and where the average number of rooms per 
##house is 6?

# median price for the new tract
reg$coef %*% c(1, 0.1, 0, 6)
# %*%: matrix multiplication
#Therefore the median house price is $20,832.32.

#the prediction error of the new data is obtained by setting se.fit = TRUE 
#in the predict() function
new <- data.frame(CHAS = 0, CRIM = 0.1, RM = 6)
pred <- predict(reg, newdata = new, se.fit = TRUE)
pred$fit
#as shown above, the median house price is therefore $20,832.32.
#prediction error
pred$se.fit
#prediction error is $334.8071

## d) Compute the correlation table for the 12 numerical predictors and 
##search for highly correlated pairs. These have potential redundancy and can
##cause multicollinearity. Choose which ones to remove based on this table.

#correlation table for the 12 numerical predictors
t(t(names(housing.df)))
cor(housing.df[,-c(4)])
cor(housing.df[,-housing.df$CHAS])
#Highly correlated pairs are as follows:
#  1) NOX and INDUS: Correlation coefficient = 0.76365
#  2) TAX and INDUS: Correlation coefficient = 0.72076
#  3) AGE and NOX: Correlation coefficient = 0.73147
#  4) DIS and NOX: Correlation coefficient = -0.76923
#  5) DIS and AGE: Correlation coefficient = -0.74788
#  6) DIS and INDUS: Correlation coefficient = 0.7080270
#  7) TAX and RAD: Correlation coefficient = 0.91022

#According to the correlation table, we might be able to remove some variables
#that do not add much information to others that we keep. We might remove 
#INDUS, AGE and TAX.

## e) Use exhaustive and stepwise regression with the three options (backward, forward, 
##both) to reduce the remaining predictors as follows: Run stepwise on the 
##training set. Choose the top model from each stepwise run. Then use each of
##these models separately to predict the validation set. Compare RMSE, MAPE, 
##and mean error, as well as lift charts. Finally, describe the best model.

library(leaps)
set.seed(1)  
train.index <- sample(c(1:dim(housing.df)[1]), 
                      0.6*dim(housing.df)[1])  
valid.index <- setdiff(c(1:dim(housing.df)[1]), train.index)  
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]

#stepwise regression with exhaustive search
search <- regsubsets(MEDV ~ .,
                     data = housing.df,
                     nbest = 1,
                     nvmax = dim(train.df)[2],
                     method = "exhaustive")

sum <- summary(search)
sum$which
# show metrics
rsq <- sum$rsq
adjr2 <- sum$adjr2
cp <- sum$cp
bic <- sum$bic
cbind(rsq,adjr2,cp,bic)
#To find the top three models, the criteria used is as follows:
#Find the 3 highest values of adjusted R-squared
t(t(sum$adjr2))
# top 3 models
models <-  order(sum$adjr2, decreasing = T)[1:3]
models

library(gains) #for gains and lift chart
library(forecast) #for accuracy measures

# run model on training and validation

par(mfcol=c(1,3))
for (model in models){
  print(model) #print model number
  selected.vars = names(train.df)[sum$which[model,-1]]
  reg.model <- lm(MEDV ~ ., data = train.df[,selected.vars])
  
  # training
  cat("training RMSE = ", accuracy(reg.model$fitted.values, train.df$MEDV)[2],"\n") #print RMSE
  cat("training MAPE = ", accuracy(reg.model$fitted.values, train.df$MEDV)[5],"\n") #print MAPE
  
  # validation
  pred <- predict(reg.model, valid.df)
  cat("validation RMSE = ", accuracy(pred, valid.df$MEDV)[2],"\n") #print RMSE
  cat("validation MAPE = ", accuracy(pred, valid.df$MEDV)[5],"\n") #print MAPE
  
  #lift charts
  gain <- gains(valid.df$MEDV, pred)
  plot(c(0, gain$cume.pct.of.total*sum(valid.df$CRIM)) ~ 
         c(0, gain$cume.obs), 
       xlab="# cases", ylab="Cumulative", main=model, type="l")
}

#Summary of top three models to choose the best model:
#Model 1 is designed using the following 10 variables: 
#  CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, PTRATIO, LSTAT. 
#Model 2 is designed using the following 11 variables: 
#  CRIM, ZN, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, LSTAT. 
#Model 3 is designed using the following 12 variables: 
#  CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, LSTAT.
#
#Note that as we add more variables, the error decreases (but the lower 
#error comes at the cost of a more complex model). 
#Overall, Model I with the variables CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, 
#PTRATIO, LSTAT is the best model (given not much difference in prediction error
#as compared to other two model and is less complex) for predicting Boston 
#Housing prices.

#Alternatively we can use step() function from package stats to perform
#stepwise regression
#stepwise regression with backward search.
#you can either leave default settings or mention direction = "backward" in 
# step() function for backward search. Both will result in backward search.
#fit the model with all the variables first.

reg2 <- lm(MEDV ~ ., data = train.df)
#backward regression
step.backward <- step(reg2, direction = "backward")
step(reg2, direction = "backward") # To see AIC measure
extractAIC(step.backward) # another way to see AIC measure

#or step.backward <- step(reg2)
backward <- summary(step.backward)
#the best model according to backward search algorithm is
#  MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT

#predictions on validation set
lm1n <- lm(backward$call[2], data = train.df)
lm1 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO 
          + LSTAT, data = train.df)
pred1.valid <- predict(lm1, data = valid.df)

acc.backward <- accuracy(pred1.valid, valid.df$MEDV)
acc.backward
#stepwise regression with forward search.
#set the direction parameter to "forward" in step() function for forward search. 
#forkward regression
reg.null <- lm(MEDV~1, data = train.df)
step.forward <- step(reg2, scope=list(lower=reg.null, upper=reg2), direction = "forward")
step.forward.n <- step(reg2, direction = "forward")
step(reg2, direction = "forward") # To see AIC measure
extractAIC(step.forward.n) # another way to see AIC measure
forward <- summary(step.forward)

#the best model according to forward search algorithm is
#MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO 
#       + LSTAT

#predictions on validation set
lm2 <- lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + 
            TAX + PTRATIO + LSTAT, data = train.df)
lm2.n <- lm(forward$call[2], data = train.df)

pred2.valid <- predict(lm2, data = valid.df)
acc.forward <- accuracy(pred2.valid, valid.df$MEDV)

#stepwise regression in both the directions.
#Best model decided according to AIC
#set the direction parameter to "both" in step() function. 

step.both <- step(reg2, direction = "both")
step(reg2, direction = "both") # To see AIC measure
extractAIC(step.both) # another way to see AIC measure

both <- summary(step.both)
#the best model according to backward search algorithm is
#Step:  AIC=930.63
#MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT

#predictions on validation set
lm3.n <- lm(both$call[2], data = train.df)
lm3 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + 
            LSTAT, data = train.df)
pred3.valid <- predict(lm3, data = valid.df)
acc.both <- accuracy(pred3.valid, valid.df$MEDV)
acc.both
acc.backward
acc.forward
extractAIC(step.both)
extractAIC(step.backward)
extractAIC(step.forward)
step.backward$call[2]
sum$which[10,]
#according to AIC criterion, exhaustive and backward search gave the same
#best model with 10 predictors: CRIM,ZN,CHAS,NOX,RM,DIS,RAD,TAX,PTRATIO and LSTAT