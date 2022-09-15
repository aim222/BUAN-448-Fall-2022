#### BUAN 448 Fall 2022
#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
cat("\014")

park <- read.csv("customer satisfaction.csv")
str(park)
#------------------------------------------
# This data file contains customer satisfaction information from an amusement park.

# Weekend: Whether the respondent visited on a weekend
# num.child: Number of children they brought with them
# distance: distance traveled to the park
# rides: satisfaction level (0-100) with the rides
# games: satisfaction level (0-100) with the games
# wait: satisfaction level (0-100) with the waiting time
# clean: satisfaction level (0-100) with the cleanliness
# overall: overall satisfaction level (0-100)


### Our goal is to figure out the drivers of overall satisfaction
heatmap(cor(park[,-1]), Rowv = NA, Colv = NA)
library(gplots)
heatmap.2(cor(park[,-1]), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(park[,-1]),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

par(mfcol = c(3,2))
plot(park$overall ~ park$distance, xlab = "distance traveled", ylab = "overall satisfaction")
plot(park$overall ~ park$rides, xlab = "rides satisfaction", ylab = "overall satisfaction")
plot(park$overall ~ park$games, xlab = "games satisfaction", ylab = "overall satisfaction")
plot(park$overall ~ park$wait, xlab = "wait satisfaction", ylab = "overall satisfaction")
plot(park$overall ~ park$clean, xlab = "clean satisfaction", ylab = "overall satisfaction")
plot(park$overall ~ park$num.child, xlab = "number of child", ylab = "overall satisfaction")

library(ggplot2)
library(gridExtra)
p1 <- ggplot(park) + geom_point(aes(x = distance, y = overall)) + 
  labs(x = "distance traveled", y = "overall satisfaction")
p2 <- ggplot(park) + geom_point(aes(x = rides, y = overall)) + 
  labs(x = "rides satisfaction", y = "overall satisfaction")
p3 <- ggplot(park) + geom_point(aes(x = games, y = overall)) + 
  labs(x = "games satisfaction", y = "overall satisfaction")
p4 <- ggplot(park) + geom_point(aes(x = wait, y = overall)) + 
  labs(x = "wait satisfaction", y = "overall satisfaction")
p5 <- ggplot(park) + geom_point(aes(x = clean, y = overall)) + 
  labs(x = "clean satisfaction", y = "overall satisfaction")
p6 <- ggplot(park) + geom_point(aes(x = num.child, y = overall)) + 
  labs(x = "number of child", y = "overall satisfaction")
grid.arrange(p1, p4, p2, p5, p3,p6, nrow = 3, ncol=2)

# histograms on the diagonal
plot(park[, -1]) #first way

library(car)
scatterplotMatrix(park[,-1]) #second way

library(GGally)
ggpairs(park[, -1])  #third way

#-----------------------------------------------------------------------
# Linear Model/regression with a single variable
#-----------------------------------------------------------------------
library(ggplot2)
p <- ggplot(data=park,aes(x=rides,y=overall)) 

p + geom_point(color = "purple") + 
  geom_smooth(formula = y ~ x, method ="lm", size=2, se = FALSE) +
  labs(x = "satisfaction level (0-100) with the rides",
       y = "overall satisfaction level (0-100)") 

# People who rate `rides` with high scores also rate the `overall satisfaction` highly.  
# The relationship between the two variables looks linear, with a positive slope.

lm1 <- lm(overall ~ rides, data=park)
lm1
# overall = -94.962 + 1.703*rides
# As expected, the slope is positive.
summary(lm1)
summary(lm1)$r.squared
# Prediction:
# If someone gives a rating of 90 for the `rides``, they are expected
# to give the following rating for overall `satisfaction`:
park[park$rides == 90,]
predict(lm1,park[9,])

# -94.962 + 1.703*90
# which comes out to be 58.308.
# Note that this relationship is one of correlation, not necessarily causation!  
# Spending a few million dollars on new rides may not necessarily increase the overall satisfaction.
# The model is only applicable for predictions. 
mean(park[park$rides == 90,]$overall)

#-----------------------------------------------------------------------
# Linear Model/regression with Multiple variable
#-----------------------------------------------------------------------

#### Table 6.3

car.df <- read.csv("ToyotaCorolla.csv")
str(car.df) #structure of the data frame

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

par(mfcol = c(2,2))
plot(car.df$Price ~ car.df$Age_08_04, xlab = "Age in months as of August 2004", ylab = "Price")
plot(car.df$Price ~ car.df$KM, xlab = "Accumulated kilometers on odometer", ylab = "Price")
plot(car.df$Price ~ car.df$HP, xlab = "Horsepower", ylab = "Price")
plot(car.df$Price ~ car.df$Weight, xlab = "Weight", ylab = "Price")

library(ggplot2)
library(gridExtra)
p1 <- ggplot(car.df) + geom_point(aes(x = Age_08_04, y = Price)) + 
  labs(x = "Age in months as of August 2004", y = "Price")
p2 <- ggplot(car.df) + geom_point(aes(x = KM, y = Price)) + 
  labs(x = "Accumulated kilometers on odometer", y = "Price")
p3 <- ggplot(car.df) + geom_point(aes(x = HP, y = Price)) + 
  labs(x = "Horsepower", y = "Price")
p4 <- ggplot(car.df) + geom_point(aes(x = Weight, y = Price)) + 
  labs(x = "Weight", y = "Price")

grid.arrange(p1, p3, p2,  p4, nrow = 2, ncol = 2)

t(t(names(car.df))) # find the column number of specific column
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

library(GGally)
ggpairs(car.df[,selected.var])  #third way

#-----------------------------------------------------------------------
# Multiple regression
#-----------------------------------------------------------------------
# use first 1000 rows of data
car.df <- car.df[1:1000, ]

# showing only the variables to be used in analysis
car.df[,selected.var] 

# Pre-processing 
# Converting the Categorical Fuel variable to Binary 

car.df$Fuel_Type.Diesel <- ifelse(car.df$Fuel_Type=="Diesel",1,0)
car.df$Fuel_Type.Petrol <- ifelse(car.df$Fuel_Type=="Petrol",1,0)

# partition data
set.seed(1)  # set seed for reproducing the partition (Always the same samples)
train.index <- sample(c(1:length(car.df$Id)), length(car.df$Id)*.6)
# train.index <- sample(c(1:1000), 600) #Author version

train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.

options(scipen = 999)
summary(car.lm)



#### Table 6.4


# use predict() to make predictions on a new set. 

library(forecast)
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 1)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
df.car.predicted <- data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)

ggplot(df.car.predicted) + geom_point(aes(x=c(1:20),y=Predicted),color='red') + 
  geom_point(aes(x=c(1:20),y=Actual),color='blue') + 
  ylab('Price Values')+ 
  xlab('sample') + 
  scale_x_continuous(minor_breaks = seq(1, 20, 1)) +
  scale_y_continuous(minor_breaks = seq(1, 30000, 500))

options(scipen=999, digits = 1)

# use accuracy() to compute common accuracy measures.

library(forecast)
accuracy(car.lm.pred, valid.df$Price)

# computing the accuracy for the train dataset

car.lm.train <- predict(car.lm, train.df)
accuracy(car.lm.train, train.df$Price)


#### Figure 6.1
par(mfrow=c(1,1))
library(forecast)
car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
length(all.residuals[which(all.residuals > -2000 & all.residuals < 2000)])/400

rdf <- as.data.frame(all.residuals)
library(ggplot2)
ggplot(rdf) + geom_histogram(aes(x = all.residuals), bins  = 25,color="black", fill="white") + 
  scale_x_continuous(breaks = seq(-7000, 5000, 1000))

