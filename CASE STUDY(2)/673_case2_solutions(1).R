## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module3_regression/case#2")

# Create data frame.
revenue.data <- read.csv("673_case2_7.csv")

# See the first 6 records of the file.
head(revenue.data)
tail(revenue.data)

## 1a.
# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
revenue.ts <- ts(revenue.data$Revenue, 
            start = c(2005, 1), end = c(2022, 4), freq = 4)

## 1b.	
#Apply the plot() function to create a data plot with the historical data.
plot(revenue.ts, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(70000, 180000), main = "Walmart Quarterly Revenues", 
     xaxt = "n",
    col = "blue", bty = "l", lwd = 2)
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))


## 2a.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(revenue.ts) = 72.
# nvalid = 16 quarters for the last 16 quarters (Q1-19 to Q4-22).
# nTrain = 56 quarters, from Q1-05 to Q4-18.
nValid <- 16
length(revenue.ts)
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
train.ts
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))
valid.ts


## 2b. 
# FIT REGRESSION MODEL WITH (1) LINEAR TREND (2) QUADRATIC (POLYNOMIAL) TREND, 
## (3) SEASONALITY, (4) LINEAR TREND AND SEASONALITY, AND
## (5) QUADRATIC TREND AND SEASONALITY.
## IDENTIFY FORECAST FOR VALIDATION PERIOD FOR EACH MODEL.

## (1) LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
train.lin <- tslm(train.ts ~ trend)

# See summary of quadratic trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred


## (2) QUADRATIC TREND MODEL.
# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred


## (3) SEASONALITY MODEL.
# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred


## (4) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred



## (5) QUADRATIC TREND AND SEASONALITY MODEL.
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- 
            forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred


## 2c. 
# Use accuracy() function to identify common accuracy measures
# for the developed forecast in the validation period.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)



## 3a. 
# FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY, 
# WITH LINEAR TREND AND SEASONALITY FOR ENTIRE DATASET. 
# FORECASTDATA, AND MEASURE ACCURACY.

## (1) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonality model.
lin.trend.season <- tslm(revenue.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 8, level = 0)
lin.trend.season.pred



## (2) LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
lin.trend <- tslm(revenue.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
lin.trend.pred <- forecast(lin.trend, h = 8, level = 0)
lin.trend.pred


## (3) QUADRATIC TREND AND SEASONALITY MODEL
# Use tslm() function to create quadratic trend and seasonality model.
quad.trend.season <- tslm(revenue.ts ~ trend + I(trend^2)+ season)

# See summary of linear trend equation and associated parameters.
summary(quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
quad.trend.season.pred <- forecast(quad.trend.season, h = 8, level = 0)
quad.trend.season.pred


## 3b. 
## COMPARE ACCURACY MEASURES OF REGRESSION FORECAST 
## WITH QUANDRATIC TREND AND SEASONALITY, AND LINEAR TREND AND
## SEASONALITY FOR ENTIRE DATA SET WITH ACCURACY MEASURES 
## OF NAIVE FORECAST AND SEASONAL NAIVE 
## FORECAST FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures
# for naive model, seasonal naive, and regression model 
# with quadratic trend and seasonality.
round(accuracy(lin.trend.season.pred$fitted, revenue.ts),3)
round(accuracy(lin.trend.pred$fitted, revenue.ts),3)
round(accuracy(quad.trend.season.pred$fitted, revenue.ts),3)
round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)

