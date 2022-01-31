#install.packages("remotes")
#install.packages("RCurl")
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("ggplot2")

#library(tidyverse)
#library(caret)
#theme_set(theme_classic())

library("ggplot2")
library("dplyr")
library("magrittr")
library("tidyverse")
library("dynlm")


# Download csv file
#download.file("https://raw.githubusercontent.com/SIDtwr/regression_uniwaterloo/main/data/covid_data.csv",destfile="/tmp/covid_data.csv",method="libcurl")

covid_data <- read.csv("C:/Users/GZDX20/Documents/uniwaterloo/regression_uniwaterloo-main/data/covid_data.csv", header = TRUE)
covid_data <- tibble::rowid_to_column(covid_data, "index")

####################### FORMAT DATAFRAME #############################

# get rid of unecessary columns 
covid_data <- covid_data[-c(4:5)]

# rename columns
colnames(covid_data) <- c('INDEX','DATE','CASES')

# format Column DATE
covid_data <- covid_data %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))

# format column CASES
covid_data <- covid_data %>%
  mutate(CASES = as.numeric(gsub(",", "", CASES)))

print(covid_data)
summary(covid_data)

####################### FORMAT DATAFRAME ##############################



# plot the data using ggplot2 and pipes
covid_data %>%
  ggplot(aes(x = DATE, y = CASES)) +
  geom_point(color = "darkorchid4") +
  labs(title = "OMICRON COVID-19 Cases",
       y = "Num of COVID Cases",
       x = "Date") + theme_bw(base_size = 15)


############ QUESTION ONE ###################

# Order 1
poly_reg1  <- lm(formula = CASES~INDEX,
                data = covid_data)
# Order 2
poly_reg2  <- lm(formula = CASES~INDEX + I(INDEX^2), 
                data = covid_data)
# Order 3
poly_reg3 <- lm(formula = CASES~INDEX + I(INDEX^2) + I(INDEX^3), 
                data = covid_data)
# Order 4
poly_reg4 <- lm(formula = CASES~INDEX + I(INDEX^2) + I(INDEX^3) + I(INDEX^4), 
                data = covid_data)

#log_model_one <- dynlm(log(CASES) ~ log(DATE), data = covid_data)


ggplot(covid_data) +
  geom_point(aes(INDEX, CASES, col = "Original")) +
  stat_smooth(method = "lm", formula = y~poly(x,1), aes(INDEX, poly_reg1$fitted.values, col = "Order 1")) +
  stat_smooth(method = "lm", formula = y~poly(x,2), aes(INDEX, poly_reg2$fitted.values, col = "Order 2")) +
  stat_smooth(method = "lm", formula = y~poly(x,3), aes(INDEX, poly_reg3$fitted.values, col = "Order 3")) +
  stat_smooth(method = "lm", formula = y~poly(x,4), aes(INDEX, poly_reg4$fitted.values, col = "Order 4")) +
  scale_colour_manual("",
                      breaks = c("Original","Order 1","Order 2","Order 3","Order 4"),
                      values = c("red","cyan","blue","orange","green")) +
  theme_bw()


new.data <- data.frame(
  INDEX = c(1, 30, 60, 90, 120)
)
predict(poly_reg4, newdata = new.data)




############ QUESTION TWO ###################


# Order 1
poly_reg1_log  <- lm(formula = CASES~log(INDEX),
                 data = covid_data)
# Order 2
poly_reg2_log  <- lm(formula = CASES~log(INDEX) + I(INDEX^2), 
                 data = covid_data)
# Order 3
poly_reg3_log <- lm(formula = CASES~log(INDEX) + I(log(INDEX)^2) + I(log(INDEX)^3), 
                data = covid_data)
# Order 4
poly_reg4_log <- lm(formula = CASES~log(INDEX) + I(log(INDEX)^2) + I(log(INDEX)^3) + I(log(INDEX)^4), 
                data = covid_data)

ggplot(covid_data) +
  geom_point(aes(INDEX, CASES, col = "Original")) +
  stat_smooth(method = "lm", formula = y~poly(log(x),1), aes(INDEX, poly_reg1_log$fitted.values, col = "Order 1")) +
  stat_smooth(method = "lm", formula = y~poly(log(x),2), aes(INDEX, poly_reg2_log$fitted.values, col = "Order 2")) +
  stat_smooth(method = "lm", formula = y~poly(log(x),3), aes(INDEX, poly_reg3_log$fitted.values, col = "Order 3")) +
  stat_smooth(method = "lm", formula = y~poly(log(x),4), aes(INDEX, poly_reg4_log$fitted.values, col = "Order 4")) +
  scale_colour_manual("",
                      breaks = c("Original","Order 1","Order 2","Order 3","Order 4"),
                      values = c("red","cyan","blue","orange","green")) +
  theme_bw()

new.data <- data.frame(
  INDEX = c(1, 30, 60, 90, 120)
)
predict(poly_reg4_log, newdata = new.data)

