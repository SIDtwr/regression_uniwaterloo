#install.packages("remotes")
#install.packages("RCurl")
#install.packages("devtools")

download.file("https://raw.githubusercontent.com/SIDtwr/regression_uniwaterloo/main/COVID-19%20daily%20case%20counts%20by%20reported%20date%20in%20Ontario%20-%20December%201%2C%202021%20to%20January%2027%2C%202022.csv",destfile="/tmp/reviews.csv",method="libcurl")
data <- read.csv("/tmp/reviews.csv")
print(data)

## Prepare taringing data 
## Split into traning and testing data



