#### BUAN 448 Fall 2022
### Week 2 Part 2
### Data and Variables, If-else statement

getwd()
setwd("C:/Users/Yuyue/Dropbox/Lehigh/Spring 2022/BIS 348/Week 2 Introduction to R/Part 2")

#### ---------------------------- Dataframe -----------------------------------

# Dataframes can contain both string vectors and numeric vectors.
# Data frame is two dimensional (A dataframe has columns and rows )

# Example: 
# "trees" dataset: contains Girth, Height and Volume for Black Cherry Trees.
?trees
str(trees)
# We can see that trees is a data frame with 31 rows and 3 columns.

# Access information from this data frame
trees[2,] # The 2nd row
trees[,2] # The 2nd column

trees[c(2, 5, 9), ] # Row 2, 5 and 9
trees[c(2, 5, 9), 1] # Row 2, 5 and 9, column 1

trees[2:3,]    # From the 2nd row to the 3rd row

trees[10:12,2] # From row 10 to 12, the second column

# Find all the rows/records with Height greater than 82
trees[trees$Height > 82,]   
# or
subset(trees, trees$Height>82)

# Example 2:
housing.df <- read.csv("WestRoxbury.csv")  # load data

# Information about this data set

dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab
str(housing.df) # list the structure of the data
summary(housing.df)  # find summary statistics for each column

# Indexing
housing.df[1:10, 1]  # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns
housing.df[5, 1:10]  # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)]  # show the fifth row of some columns
housing.df[, 1]  # show the whole first column

# How to remove rows? (use negative index)

# How to remove variables? (negative index or NULL)

##
housing.df$TAX <- 5 # Replace all the data in a field with a number
#
housing.df$YR.BUILT[housing.df$YR.BUILT>=1900] <- "new house" # Replace the data in a field based on equal to some value
housing.df$YR.BUILT[housing.df$ROOMS>=6] <- "big house" # Replace the data in a field based on equal to some value
# Create a new column/variable
housing.df$GOOD.HOUSE[housing.df$FLOORS==2] <- 1
housing.df$GOOD.HOUSE[is.na(housing.df$GOOD.HOUSE)] <- 0


###  ----------  If-else statements --------------------------------------

# There are two types of if statements in R. 
###  1. The simple "if statement":

x <- 10

if (x > 20) {
  y <- 1
}

# We can use this in conjunction with an "else" statement:
x <- 18

if (x > 20) {
  y <- 1
} else {
  y <- 0
}

# We can include multiple commands:
x <- 20

if (x == 21) {
  y <- 1
  w <- TRUE
} else {
  y <- 0
  w <- FALSE
}

y
w


# Exercise: 
# if x > 10, y = 1, z = "TRUE"
# if x == 10, y = 2, z = "FALSE"
# if x < 10, y = 3, z = "NA"

x <- 4

if (x > 10) {
  y <- 1
  w <- "TRUE"
} else if (x == 10) {
  y <- 2
  w <- "FALSE"
} else {
  y <- 3
  w <- "NA"
}

y
w



#### 2. "ifelse" statement

# ifelse(Boolean_Vector, Outcome_If_True, Outcome_If_False)

# If the condition is satisfied, Outcome_If_True is returned.  If the
# condition is not satisfied, Outcome_If_False is returned.

ifelse(c("True", "False", "True", "False"), "Young", "Old")

ifelse(3 > 4, x <- 5, x <- 6)




