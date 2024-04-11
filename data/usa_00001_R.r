# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
library(haven)
setwd("/Users/hobuobi/downloads/")
data <- read_dta("usa_00002.dta")
data <- subset(data, valueh != 9999998 & valueh != 9999999)
data$bedrooms <- ifelse(data$bedrooms != 0, data$bedrooms - 1, NA)

# dummy variables: bedrooms
data$bd_0 <- ifelse(data$bedrooms == 0, 1, 0)
data$bd_1 <- ifelse(data$bedrooms == 1, 1, 0)
data$bd_2 <- ifelse(data$bedrooms == 2, 1, 0)
data$bd_3 <- ifelse(data$bedrooms == 3, 1, 0)
data$bd_4 <- ifelse(data$bedrooms == 4, 1, 0)
data$bd_5 <- ifelse(data$bedrooms == 5, 1, 0)

# dummy variables: condo
data$condo <- ifelse(data$condo != 0, data$condo - 1, NA)

# modeling
reg_linear <- lm(valueh ~ bedrooms + condo,data, na.action = na.omit)
reg_dummy <- lm(valueh ~ bd_0 + bd_1 + bd_2 + bd_3 + bd_4 + bd_5 + condo, data, na.action = na.omit)
