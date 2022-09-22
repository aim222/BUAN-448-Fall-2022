#### BUAN 448 Fall 2022
#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
cat("\014")

#### Table 6.3

car.df <- read.csv("ToyotaCorolla.csv")
# Variable
# Price : Offer price in Euros
# Age: Age in months as of August 2004 
# Kilometers:  Accumulated kilometers on odometer 
# Fuel type: (Petrol, Diesel, CNG)
# HP: Horsepower 
# Metallic: Metallic color? (Yes = 1, No = 0)
# Automatic: Automatic (Yes = 1, No = 0)
# CC: Cylinder volume in cubic centimeters
# Doors: Number of doors
# QuartTax: Quarterly road tax in Euros
# Weight: Weight in kilograms

# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

# only needed columns will be used
car.df <- car.df[,selected.var]


# partition data
set.seed(40)  # set seed for reproducing the partition

train.index <- 
  sample(c(1:length(car.df$Price)), length(car.df$Price)*.6)

train.df <- car.df[train.index, ]
valid.df <- car.df[-train.index, ]

#### Table 6.5

# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

# show metrics
rsq <- sum$rsq
adjr2 <- sum$adjr2
cp <- sum$cp
bic <- sum$bic
cbind(rsq,adjr2,cp,bic)

# show models
sum$which

sum$outmat

#### Table 6.7
# create model with no predictors
car.lm.null <- lm(Price~1, data = train.df)
summary(car.lm.null)
# create model with all predictors
car.lm.full <- lm(Price~., data = train.df)
summary(car.lm.full)

# use step() to run forward regression.
car.lm.step.forward <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm.full), direction = "forward")
summary(car.lm.step.forward)  # Which variables were added?
car.lm.step.pred.forward <- predict(car.lm.step.forward, valid.df)
library(forecast)
accuracy(car.lm.step.pred.forward, valid.df$Price)


#### Table 6.6
# use step() to run stepwise regression.
car.lm.step.backward <- step(car.lm.full, direction = "backward")
summary(car.lm.step.backward)  # Which variables were dropped?
car.lm.step.pred.backward <- predict(car.lm.step.backward, valid.df)
accuracy(car.lm.step.pred.backward, valid.df$Price)



#### Table 6.8
# use step() to run stepwise regression.
car.lm.step <- step(car.lm.full, direction = "both")
summary(car.lm.step)  # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
