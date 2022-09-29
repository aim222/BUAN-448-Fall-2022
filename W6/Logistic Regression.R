
rm(list=ls()) # Clear the environment
cat("\014") # clear the console


par(mfcol=c(1,1))
x <- seq(-10, 10, by = 0.01)
plot(x,exp(x), main = "exponential value")

library(ggplot2)
p <- ggplot(data.frame(cbind(x,exp=exp(x))), mapping = aes(x=x,y=exp))
p+geom_point()

par(mfcol=c(2,1))
p <- seq(0, .9, by = 0.001)
plot(p,p/(1-p), main = "odds as a function of p") # the odds as a function of p.
plot(p,log(p)-log(1-p), main = "logit as a function of p") # logit as a function of p

# ========================================================================
#                    LOGIT, Model with a Single Predictor
# ========================================================================


bank.df <- read.csv("UniversalBank.csv")
names(bank.df)
View(bank.df)

#drop ID and zip code columns: why?
bank.df <- bank.df[ , -c(1, 5)] 

#explore the data
dim(bank.df)
head(bank.df)
str(bank.df)
summary(bank.df)

#=========================== Partition data =====================================
set.seed(12345)
# What does the following line mean?
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)

train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# ========================= logistic regressions ================================
#use glm() with family = "binomial": binary classification
#a very simple model first
logit.reg1 <- glm(Personal.Loan ~ Income, data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg1)
# What's the logit function here?

odds <- exp(coef(logit.reg1)) 
odds



# ================ use predict() to compute predicted probabilities ==========

# The type="response" option tells R to output probabilities of the form P(Y = 1|X), 
logit.reg1.pred <- predict(logit.reg1, valid.df[,-8], type = "response")
logit.reg1.pred

valid.df$predicted <- logit.reg1.pred
ggplot(valid.df,aes(x=Income,y=predicted)) + geom_point()
ggplot(valid.df,aes(x=Income,y=predicted)) + geom_line()

data.frame(actual = valid.df$Personal.Loan, predicted = round(logit.reg1.pred,4))

# What shall we do next?


# ================================= Data Preprocessing =====================

as.factor(bank.df$Education)
#treat "Education" as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))
bank.df$Education

