# BUAN 448 Week 12 Chapter 14

rm(list=ls())

install.packages("arules")
library(arules)

fp.df <- read.csv("Faceplate.csv")
str(fp.df)

#remove first column and convert to matrix
fp.mat <- as.matrix(fp.df[, -1])
fp.mat

#convert the binary incidence matrix into a transactions database
fp.trans <- as(fp.mat, "transactions")
fp.trans

inspect(fp.trans)

#get rules, running apriori algorithm
rules <- apriori(fp.trans, parameter = list(support = 0.2, conf = 0.2, target = "rules"))

#inspect all the rules:
inspect(rules)

#inspect the first 6 rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 6))

inspect(head(sort(rules, by = "lift")))

# ================= Example 1: Items ===============
items.df <- read.csv("Items.csv")
items.mat <- as.matrix(items.df[, -1])
items.trans <- as(items.mat, "transactions")
inspect(items.trans)
rules <- apriori(items.trans, parameter = list(support = 0.001, conf = 0.001, target = "rules"))

rules.tbl <- inspect(rules)
rules.tbl[rules.tbl$support >= 0.04 & rules.tbl$confidence >= 0.7,]

# ========================= BookStore Example  =================

all.books.df <- read.csv("CharlesBookClub.csv")
t(t(names(all.books.df)))
# create a binary incidence matrix
count.books.df <- all.books.df[, 8:18]
summary(count.books.df)
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
summary(incid.books.df)
incid.books.mat <- as.matrix(incid.books.df[, -1])

#  convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans)

# plot data
itemFrequencyPlot(books.trans)

# run apriori function
rules <- apriori(books.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))

# inspect rules
inspect(sort(rules, by = "lift"))

# Italian cooking books are simply a subset of cookbooks
all.books.df[,c(10,15)]