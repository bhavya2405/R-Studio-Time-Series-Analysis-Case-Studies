
library(forecast)
library(zoo)



###Question 1

#PART A

sales.data <- read.csv("673_case1.csv")


sales.ts <- ts(sales.data$Sales, 
                   start = c(2015, 1), end = c(2022, 12), freq = 12)


#PART B

plot(sales.ts, 
     xlab = "Month", ylab = "Sales", 
     ylim = c(100, 500),
     main = "Sales in months")

axis(1, at = seq(2015, 2022, 1), labels = format(seq(2015, 2022, 1)))



#PART C

autocor <- Acf(sales.ts, lag.max = 12, 
               main = "Autocorrelation for Monthly sales")



Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)




#######Question 2


#PART A
nValid <- 24
nTrain <- length(sales.ts) - nValid
train.ts <- window(sales.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.ts <- window(sales.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))


#PART B
ma.trailing_3 <- rollmean(train.ts, k = 3, align = "right")
ma.trailing_8 <- rollmean(train.ts, k = 8, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")


#PART C
ma.trail_3.pred <- forecast(ma.trailing_3, h = nValid, level = 0)
ma.trail_3.pred
ma.trail_8.pred <- forecast(ma.trailing_8, h = nValid, level = 0)
ma.trail_8.pred
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

#PART D

round(accuracy(ma.trail_3.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_8.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)


plot(sales.ts, 
     xlab = "Month", ylab = "Sales", 
     ylim = c(100, 500), bty = "l", xaxt = "n",
     xlim = c(2015, 2025), main = "Trailing Moving Average") 
axis(1, at = seq(2015, 2025, 1), labels = format(seq(2015, 2025, 1)) )
lines(ma.trailing_3, col = "brown", lwd = 2, lty = 1)
lines(ma.trail_3.pred$mean, col = "blue", lwd = 2, lty = 1)

legend(2015,540, legend = c("SALES DATA", 
                             "Trailing MA, k=4, Training Partition", 
                             "Trailing MA, k=4, Validation Partition"), 
       col = c("black", "brown", "blue"), 
       lty = c(1, 1, 1), lwd =c(1, 2, 2), bty = "n")


lines(c(2020, 2020), c(0, 500))
lines(c(2023, 2023), c(0, 500))
text(2017, 400, "Training")
text(2021.5, 400, "Validation")
text(2024.4, 400, "Future")
arrows(2015, 400, 2019.9, 400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 400, 2022.9, 400, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 400, 2025.3, 400, code = 3, length = 0.1,
       lwd = 1, angle = 30)




####################QUESTION 3


#PART A

trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

#PART B
trend.seas.res <- trend.seas$residuals
trend.seas.res

ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res



ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred


#PART c


fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level



table.df <- round(data.frame(valid.ts, trend.seas.pred$mean, ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(table.df) <- c("Validation.data","Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
table.df



round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)


#PART D

fut.trend.seas <- tslm(sales.ts ~ trend  + season)

fut.trend.seas.res <- fut.trend.seas$residuals

fut.ma.trail.res <- rollmean(fut.trend.seas.res, k = 3, align = "right")

fut.trend.seas.pred <- forecast(fut.trend.seas, h = 12, level = 0)

fut.ma.trail.res.pred <- forecast(fut.ma.trail.res, h = 12, level = 0)

fut.fst.2level <- fut.trend.seas.pred$mean + fut.ma.trail.res.pred$mean

fut12.df <- round(data.frame(fut.trend.seas.pred$mean, fut.ma.trail.res.pred$mean, 
                             fut.fst.2level), 3)
names(fut12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
fut12.df



#PART E


prod.snaive.pred <- snaive(sales.ts, h=length(sales.ts))

round(accuracy(prod.snaive.pred$fitted, sales.ts),4)

# accuracy for regression model with trend and Seasonality
round(accuracy(fut.trend.seas.pred$fitted, sales.ts),4)

# accuracy for two-level model
round(accuracy(fut.trend.seas.pred$fitted + fut.ma.trail.res, sales.ts),4)

#########Question 4

#PART A

hw.sales.zzz <- ets(train.ts, model = "ZZZ")
hw.sales.zzz

hw.sales.zzz.pred <- forecast(hw.sales.zzz, h = nValid, level = 0)
hw.sales.zzz.pred

#PART B

tot.hw.sales.zzz <- ets(sales.ts, model = "ZZZ")
tot.hw.sales.zzz

comp.tot.hw.sales.zzz <- ets(sales.ts, model = "ANA")
comp.tot.hw.sales.zzz


tot.hw.pred.zzz <- forecast(tot.hw.sales.zzz, h = 12 , level = 0)
tot.hw.pred.zzz

comp.hw.pred.zzz <- forecast(comp.tot.hw.sales.zzz, h = 12 , level = 0)
comp.hw.pred.zzz


#PART C

round(accuracy(snaive(sales.ts)$fitted, sales.ts),3)
round(accuracy(tot.hw.sales.zzz$fitted, sales.ts), 3)
round(accuracy(comp.tot.hw.sales.zzz$fitted, sales.ts), 3)






