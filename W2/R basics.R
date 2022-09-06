#### Introduction to R 
#### BIS 448 Week 2 Part 1

#### ---------------------------- Working directory --------------------------
# Get your current working directory
getwd()

# Set your working directory manually
setwd("~/Library/CloudStorage/OneDrive-RutgersUniversity/@Teaching/+Fall2022/Lehigh/BIS448/1PPT/W2")

# Files -> More -> Set as working directory

#### ---------------------------- R: The Basics ------------------------------

# Commands
# Commands are separated by a line break
# As shown above, the '#' is used as a comment or notes

# Basic Operations:
# RStudio can be used as a calculator using both numbers and named variables

### Workflow Basics
3 * 4
x <- 3 * 4
x
print(x)

# Case Sensitivity
A <- 2
a

a <- 3
a

# To confirm, A and a are not the same (not equal):
A == a

# Addition
1+1
1+a
a+A


# Subtraction
10-1
10-A
A-a

# Multiplication
2*3
A*2
7*a
a*A

# Division (forward slash: /)
3/2
16/A
A/a

# Exponent
# We can either use '^' or '**' to denote an exponent
3^a
3**a

### Remove an Assignment/item/variable
## Overwrite the variable
t1 <- NULL
t1

## Use the rm() function
?rm
rm(a)

### Clearing the Global Environment (also called the Workspace)
rm(list=ls())

#### ---------------------------- Built-in functions ---------------------------

## We can use '?' or 'help()' to obtain help for a function
# The help information will be displayed in the 'Help' panel
# Example: print()
?print
help(print)

## We can use example() to see an example of a functions use
example(print)

### Example: seq()
?seq
seq(from = 1, to = 10, by = 2)
seq(1, 10, 2)

help(seq)

example(seq)


#### ---------------------------- Packages -----------------------------------
### install.packages() or "Packages" command on the right side

##### Example: Titanic Data
titanic
# Error: object 'titanic' not found
install.packages("socviz")
library(socviz)
titanic
?titanic

class(titanic)
str(titanic)
summary(titanic)

my_packages <- c("tidyverse", "broom", "coefplot", "cowplot", "drat",
                 "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                 "here", "interplot", "margins", "maps", "mapproj",
                 "mapdata", "MASS", "quantreg", "rlang", "scales", "socviz",
                 "survey", "srvyr", "viridis", "viridisLite", "devtools",
                 "rtools","backports","vctrs")


install.packages(my_packages, repos = "http://cran.rstudio.com")

# Install packages not yet installed
installed_packages <- my_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(my_packages[!installed_packages])
}

#### ---------------------------- Vector ---------------------------------------

# A vector is a sequence of values, all of the same type!
# Vectors are generally created by the c() function.
c(1,2,3,1,3,5,25)
my_numbers <- c(1,2,3,1,3,5,25)
your_numbers <- c(5,31,71,1,3,21,6)
my_numbers
print(my_numbers)

x <- c(2,4,6,8,10,6,7,8,9)
# To get the mean/average of x, use the mean() function
mean(x)
mean(my_numbers)

# Access element from a vector:
x[3]
# Display the first 4 elements 
x[1:4]
# colon operator ":" gets a sequence of numbers from... to...

# Display the first and the third element 
x[c(1, 3)]
x[c(1, 3, 4)]
# Negative index:
x[-1]
y <- x[c(-1, -3)]

## Numbers in R:
# NAN (not a number)
# NA (missing value)
x <- c(1,  2,  3 , 4  ,5 , 6 , 7 , 8,  NA)
mean(x)
?mean()
mean(x, na.rm = TRUE)

# Example:
my_numbers <- c(1,2,3,1,3,5,25)
sd(my_numbers) # standard deviation
my_numbers
my_numbers * 5
my_numbers + 1
my_numbers + my_numbers

#### ---------------------------- Load data into R ---------------------------
# Importing a local csv file
install.packages("readr") # Install package
library("readr") # Load package
housing.df <- read_csv("WestRoxbury.csv")

# Note: The above R code assumes that the file WestRoxbury.csv is in your current working directory. 
read_csv("~/Desktop/BUAN448/BUAN-448-Fall-2022/W2/WestRoxbury.csv")

# file.choose()
mydata.df <- read.csv(file.choose())
str(mydata.df)
summary(mydata.df)

#### Importing/reading Excel file 
install.packages("readxl")
library("readxl")

goog_close.df <- read_excel(file.choose())


### --------------------------- Save your work----------------------------------
getwd()
setwd("~/Library/CloudStorage/OneDrive-RutgersUniversity/@Teaching/+Fall2022/Lehigh/BIS448/1PPT/W2")

### .RData file
save.image("Week2_part1.RData") 

