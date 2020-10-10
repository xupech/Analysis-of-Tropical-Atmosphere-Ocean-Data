library(tidyverse)
library(forecast)
library(zoo)
library(leaps)
library(dplyr)
library(dummies)

new_elnino <- read_csv("C:/Users/Hao Long/Desktop/Spring 2020/Big Data II/3.3.2020 Assignment/new-elnino.csv")

new_elnino <-  new_elnino %>% 
  filter (buoy == 2)
new_elnino <- transform(new_elnino, `New Date` = as.Date(as.character(`New Date`), "%Y%m%d"))
new_elnino$MonthN <- as.numeric(format(as.Date(new_elnino$New.Date),"%m")) # Month's number
new_elnino <- cbind(new_elnino, dummy(new_elnino$MonthN, sep = "_"))

########## Trend and seasonality exploration
# full dataset 19900501 - 19980609
# plot(new_elnino$`Sea Surface Temp`)
# the series has some seasonality. Because the data are missing, it's hard to choose a frequency
# new_elnino.ts <- ts(new_elnino$`Sea Surface Temp`, frequency = 280)
# decomposedRes <- decompose(new_elnino.ts, "additive")
# plot (decomposedRes)

########## Data partition
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:2298), 1500)  
selected.var <- c(5,6,7,8,9)
train.t <- new_elnino[train.index, selected.var]
valid.t <- new_elnino[-train.index, selected.var]

########## Eliminate outliers
train.t <- train.t[-c(681,319,1392,609,1443,792,919,781,1391,872,520,718,481,694,461,812,1028,920,976,1169,1026,621,
                      159,62,390,633,545,1017,1461,314,710,574,930,1134,116,158,351,306,839,391,1490,793,1102,1395,625,
                      1151,1094,405,1050,1481,748,43,869,1086,787,968,290,412,927,760,426,222,631,387,1054,
                      798,601,602,830,398,851,1449,638,525,52,953,211,1417,1231,118,104,1474,886,498,109,1434,825),]

########## Regression
linearmodel <- lm(Sea.Surface.Temp ~ Zonal.Winds + Meridional.Winds + Humidity + Air.Temp, data = train.t)
options(scipen = 999)
summary(linearmodel)
vif(linearmodel)

##########  Prediction
linearmodel.pred <- predict(linearmodel, valid.t)
all.residuals <- valid.t$Sea.Surface.Temp - linearmodel.pred
# Adjusted R Squared: 0.9602
# RMSE: 0.4092914
accuracy(linearmodel.pred, valid.t$Sea.Surface.Temp)

##########  Residuals plotting
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
par(mfrow = c(2, 2))
plot(linearmodel)


################################################### Add New Variables ###################################################
# Although the data were not collected evenly every year, we still spotted that there could be seasonality in the series.
# We added monthly dummy variables in our dataset and ran the regression with dummies.

selected.var.new <- c(5,6,7,8,9,14,15,16,17,18,19,20,21,22,23,24)
train.t.new <- new_elnino[train.index, selected.var.new]
valid.t.new <- new_elnino[-train.index, selected.var.new]

# Eliminate same outliers
train.t.new <- train.t.new[-c(681,319,1392,609,1443,792,919,781,1391,872,520,718,481,694,461,812,1028,920,976,1169,1026,621,
                              159,62,390,633,545,1017,1461,314,710,574,930,1134,116,158,351,306,839,391,1490,793,1102,1395,625,
                              1151,1094,405,1050,1481,748,43,869,1086,787,968,290,412,927,760,426,222,631,387,1054,
                              798,601,602,830,398,851,1449,638,525,52,953,211,1417,1231,118,104,1474,886,498,109,1434,825),]

# Regression
linearmodel.new <- lm(Sea.Surface.Temp ~ Zonal.Winds + Meridional.Winds + Humidity + Air.Temp 
                      +new_elnino_2+new_elnino_3+new_elnino_4+new_elnino_5+new_elnino_6+new_elnino_7
                      +new_elnino_8+new_elnino_9+new_elnino_10+new_elnino_11+new_elnino_12, data = train.t.new)
options(scipen = 999)
summary(linearmodel.new)

linearmodel.new.pred <- predict(linearmodel.new, valid.t.new)
all.residuals.new <- valid.t.new$Sea.Surface.Temp - linearmodel.new.pred

# Adjusted R Squared: 0.9638

# RMSE: 0.3970702
accuracy(linearmodel.new.pred, valid.t.new$Sea.Surface.Temp)

par(mfrow = c(2, 2))
plot(linearmodel.new)