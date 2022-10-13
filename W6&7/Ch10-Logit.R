
rm(list=ls()) # Clear the environment
cat("\014") # clear the console


par(mfcol=c(1,1))
x <- seq(0, 1, by = .02)
plot(x,1/(1+exp(-x)), xlab = "Values", ylab = "probability")

plot(x,exp(x), main = "Odds vs Probability", xlab = "Probability of success", ylab = "Odds")

library(ggplot2)
p <- ggplot(data.frame(cbind(x,exp=exp(x))), mapping = aes(x=x,y=exp))
p+geom_point()

par(mfcol=c(2,1))
p <- seq(0, .99999, by = 0.001)
plot(p,p/(1-p), main = "odds as a function of p") # the odds as a function of p.
plot(p,log(p)-log(1-p), main = "logit as a function of p") # logit as a function of p
par(mfcol=c(1,1))
plot(p,log(p)-log(1-p),xlab = "Probability of success", ylab = "Logit") # logit as a function of p
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

#=========================== Partition data ===========================
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
coef(logit.reg1)

#Classify a person with income 180,000. whether s/he would be approved for a loan
logit.reg1$coef %*% c(1, 180)
odds <- exp(coef(logit.reg1) %*% c(1, 180)) 
odds/(1+odds)
# or 
new <- data.frame(Income = 180)
predict(logit.reg1, newdata = new, type = "response")

# Check some conditions for computing coefficients
sum(bank.df$Personal.Loan==0)
sum(bank.df$Personal.Loan==1)
sum(bank.df$Personal.Loan==1)/sum(bank.df$Personal.Loan==0)

# ================ use predict() to compute predicted probabilities ==========

# The type="response" option tells R to output probabilities of the form P(Y = 1|X), 
logit.reg1.pred <- predict(logit.reg1, valid.df[,-8], type = "response")
logit.reg1.pred

valid.df$predicted <- logit.reg1.pred #Add new column (predicted) to the dataframe
ggplot(valid.df,aes(x=Income,y=predicted)) + geom_point()
ggplot(valid.df,aes(x=Income,y=predicted)) + geom_line()

logi.df <- data.frame(actual = valid.df$Personal.Loan, predicted = round(logit.reg1.pred,4))


# ================================= Data Preprocessing =====================

as.factor(bank.df$Education)
#treat "Education" as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))
levels(bank.df$Education)
set.seed(12345)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)

train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]
# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

# ====================================================
# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

# actual and predicted records
act.pre <- data.frame(actual = valid.df$Personal.Loan, predicted = logit.reg.pred)
act.pre$row <- c(1:length(act.pre$actual))

ggplot(act.pre[c(80:160),], aes(row)) + 
  geom_point(aes(y = actual, colour = "actual")) + 
  geom_point(aes(y = predicted, colour = "predicted")) + ylab("") + 
  theme(legend.position="top")
# What shall we do next?
# ======================================================
#========================confusion matrix===================================
install.packages("caret")
library(caret)
cmatrix <- confusionMatrix(as.factor(valid.df$Personal.Loan),as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)))
cmatrix
# What does this confusion matrix mean?
# Output the accuracy only:
cmatrix$overall[1]
cmatrix$overall[[1]]
# Is 0.5 the best cutoff value?
df <- NULL

for (i in seq(0.1,.99,0.1)){
  print(i)
  cmatrixi <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > i, 1, 0)))$overall[[1]]
  df <- rbind(df,data.frame(cutoff=i,cmatrix=cmatrixi))
}
library(ggplot2)
p <- ggplot(data = df, mapping = aes(x =cutoff, y=cmatrix))
p + geom_point() + 
  geom_line()+
  labs(x = "Cutoff",
       y = "Accuracy",
       title = "Model Prediction Accuray using Different Cutoff Values")
# Question: Which cutoff value shall we use then?
# ======= another way to compute confusion matrix without for loop ===========
names(df) <- c("cutoff", "cmatrix")
# Define different cutoff values and compare model accuracy
cmatrix0.1 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.1, 1, 0)))$overall[1]
cmatrix0.2 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.2, 1, 0)))$overall[1]
cmatrix0.3 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.3, 1, 0)))$overall[1]
cmatrix0.4 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.4, 1, 0)))$overall[1]
cmatrix0.5 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)))$overall[1]
cmatrix0.6 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.6, 1, 0)))$overall[1]
cmatrix0.7 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.7, 1, 0)))$overall[1]
cmatrix0.8 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.8, 1, 0)))$overall[1]
cmatrix0.9 <- confusionMatrix(as.factor(valid.df$Personal.Loan), as.factor(ifelse(logit.reg.pred > 0.9, 1, 0)))$overall[1]


cutoff <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
cmtraix.all <- c(cmatrix0.1, cmatrix0.2, cmatrix0.3, cmatrix0.4, cmatrix0.5, cmatrix0.6,
                 cmatrix0.7, cmatrix0.8, cmatrix0.9)
library(ggplot2)
p <- ggplot(data = data.frame(cmtraix.all, cutoff), mapping = aes(x = cutoff, y=cmtraix.all))
p + geom_point() + 
  geom_line()+
  labs(x = "Cutoff",
       y = "Accuracy",
       title = "Model Prediction Accuray using Different Cutoff Values")
# ==================================================

# ==========================plot lift chart==================================
install.packages("gains")
library(gains)
logit.reg.gain <- gains(valid.df$Personal.Loan, logit.reg.pred)
logit.reg.gain

# plot lift chart

plot(c(0, logit.reg.gain$cume.pct.of.total*sum(valid.df$Personal.Loan)) ~ 
       c(0, logit.reg.gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", type="l",xaxp = c(0, 2000, 10))
# Can you explain this figure?

# cumulative percent of total response * The total number of Class 1 (actual) in the validation set 
# Representing "the cumulative number of responses (predicted correctly as Class 1) in the validation set by the logistic model"

# Add baseline model:
x_baseline <- c(0, dim(valid.df)[1]) # length of the validation set
y_baseline <- c(0, sum(valid.df$Personal.Loan)) # The total number of Class 1 (actual) in the validation set
lines(x_baseline, y_baseline, lty=2)



#compute deciles and plot decile-wise chart
sum(logit.reg.sort$Actual.Class[1:200])
sum(logit.reg.sort$Actual.Class[200:400])

heights <- logit.reg.gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = logit.reg1.gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

# =======================Variable Selection =====================

step.backward <- step(logit.reg, direction = "backward")
# reg.null <- glm(Personal.Loan~1, data = train.df)
# step.forward <- step(logit.reg, scope=list(lower=reg.null, upper=logit.reg), direction = "forward")
step.forward <- step(logit.reg, direction = "forward")
step.both <- step(logit.reg, direction = "both")

# ===========================Example Delayed Flights =======================
delays.df <- read.csv("FlightDelays.csv")
barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$DAY_WEEK), 
                        mean, rm.na = T)[,2], xlab = "Day of Week", ylab = "Average Delay", 
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$DEST), 
                  mean, rm.na = T)[,2], xlab = "Destination", ylab = "Average Delay", 
        names.arg = levels(as.factor(delays.df$DEST)) )
barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$ORIGIN), 
                  mean, rm.na = T)[,2], xlab = "ORIGIN", ylab = "Average Delay", 
        names.arg = levels(as.factor(delays.df$ORIGIN)) )

barplot(aggregate(delays.df$Flight.Status == "delayed", by = list(delays.df$CARRIER), 
                  mean, rm.na = T)[,2], xlab = "CARRIER", ylab = "Average Delay", 
        names.arg = levels(as.factor(delays.df$CARRIER)) )


library(reshape)
library(ggplot2)
# create matrix for plot
delays.df$isDelay <-  ifelse(delays.df$Flight.Status == "delayed",1,0)

agg <- aggregate(delays.df$isDelay, 
                 by = list(delays.df$DAY_WEEK, delays.df$CARRIER, delays.df$ORIGIN), 
                 FUN = mean, na.rm = TRUE)
m <- melt(agg)
names(m)[1:3] <- c("DAY_WEEK", "CARRIER", "ORIGIN")

# plot with ggplot
# use facet_grid() with arguments scales = "free" and space = "free" to skip 
# missing values.
ggplot(m, aes(y = CARRIER, x = DAY_WEEK, fill = CARRIER)) +  geom_tile() + 
  facet_grid(ORIGIN ~ ., scales = "free", space = "free") + 
  scale_fill_gradient(low="white", high="black")

