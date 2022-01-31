#install.packages("remotes")
#install.packages("RCurl")
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("caret")
install.packages("ggplot2")

#library(tidyverse)
#library(caret)
#theme_set(theme_classic())

library("ggplot2")

# Download csv file
download.file("https://raw.githubusercontent.com/SIDtwr/regression_uniwaterloo/main/data/covid_data.csv",destfile="/tmp/covid_data.csv",method="libcurl")

data <- read.csv("/tmp/covid_data.csv")
print(data)

## Visulize Dataset
ggplot(data) + 
  geom_point(data(Date, Cases),size=3) + 
  theme_bw()

## Prepare training data & splitting into training and testing dataset

# Set a seed value for reproducible results
set.seed(70)
# Split the data
ind <- sample(x = nrow(data), size = floor(0.75 * nrow(data))
# Store the value in train and test dataframes
train <- data[ind,]
test <- data[-ind,]

# Generate the model
model_one <- dynlm(Cases ~ Date, data = train)
model_two <- dynlm(Cases ~ Date + I(Date^2), data = train)
model_three <- dynlm(Cases ~ Date + I(Date^2) + I(Date^3), data = train)
model_four <- dynlm(Cases ~ Date + I(Date^2) + I(Date^3) + I(Date^4), data = train)

# Website links
# https://www.pluralsight.com/guides/polynomial-functions-analysis-with-r
# http://www.sthda.com/english/articles/40-regression-analysis/162-nonlinear-regression-essentials-in-r-polynomial-and-spline-regression-models/

## Test 
# Load the data
#data("Boston", package = "MASS")
# Split the data into training and test set
#set.seed(123)
#training.samples <- Boston$medv %>%
  #createDataPartition(p = 0.8, list = FALSE)
#train.data  <- Boston[training.samples, ]
#test.data <- Boston[-training.samples, ]



