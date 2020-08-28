Belfast_Crm <- read.csv("D:/DS/CA3/Belfast_Crm.csv")
#Merging Calendar_Year and Month Columns
Belfast_Crm$Date = paste(Belfast_Crm$Calendar_Year,Belfast_Crm$Month, sep = "-")
str(Belfast_Crm)
#Converting Date to type Date
library("zoo")
Belfast_Crm$ Date <- as.Date(as.yearmon(Belfast_Crm$Date, format = "%Y-%m"))
#Removing the unwanted columns 
Belfast_Crm <- Belfast_Crm[ -c(1, 2, 4:6, 12) ]
#Moving the Date Column to first
Belfast_Crm <- Belfast_Crm[,c(2,1:ncol(Belfast_Crm)-1)]

#Creating a time series object
Dealt_Rate <- ts(Belfast_Crm$CD_Rate, start=c(2001, 4), frequency=12)
#Displaying the time series object
Dealt_Rate
#Show the cycle of ts object
cycle(Dealt_Rate)

#Using functions to determine various properties of the time series object.
#This information shows that the time series object contains data between 2001 to 2019. 
#The frequency indicates the data is monthly.
cat("Start of Dealt_Rate : ", start(Dealt_Rate), "\n")
cat("End of Dealt_Rate : ", end(Dealt_Rate), "\n")
cat("Frequency of Dealt_Rate : ", frequency(Dealt_Rate), "\n")
cat("Class of Dealt_Rate : ", class(Dealt_Rate), "\n")
print(summary(Dealt_Rate))


#Plotting the data 
plot(Dealt_Rate, xlab="Year", ylab = "Dealt Rate(Percentage)",main="Raw time series")
# Adding a straight line that shows the linear relationship
# between Dealt Rate and time
#****************
abline(reg=lm(Dealt_Rate~time(Dealt_Rate)))
#****************

#We can examine the yearly trend within the data using the aggregate() function.
#The chart demonstrates the year based trend, that the delat rate is increasing.
plot(aggregate(Dealt_Rate,FUN=mean))


#install.packages("forecast")
library(forecast)
ylim <- c(min(Dealt_Rate), max(Dealt_Rate))
plot(Dealt_Rate, xlab="Year", ylab = "Dealt Rate(Percentage)",main="Raw time series")



#We can examine any seasonal effects within the data using a boxplot()
#In the boxplot there are more criminal damage cases delat in months of january, june, October, November and December
#This indicates seasonality with cycle of 12 months. 
boxplot(Dealt_Rate ~ cycle(Dealt_Rate),
        xlab="Date", 
        ylab = "Crime Dealt Rate (%)" ,
        main ="Monthly Crime Dealt Rate in Belfast_City from 2001 to 2019")


#Decomposing the trend, seasonality and residuals using the stl()
Dealt_Rate_stl <- stl(Dealt_Rate, s.window="period")
#Removing the seasonal component
Dealt_Rate_sa <- seasadj(Dealt_Rate_stl)
#Plotting the stl()
plot(Dealt_Rate_stl)
#Plotting the original data, seasonality removed data and seasonality for differentt years 
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,3))
plot(Dealt_Rate, type="l")  # original series
plot(Dealt_Rate_sa, type="l")  # seasonal adjusted
seasonplot(Dealt_Rate_sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Dealt Crime") # seasonal frequency set as 12 for monthly data.
par(opar)


#We can apply the adf.test() function, which the Augmented Dickey-Fuller Test 
#to determine if a time series is stationary
# Test if a time series is stationary
library(tseries)
# p-value < 0.05 indicates the TS is stationary
# The Dickey-Fuller value obtined is -4.4228
# In this eample,  Belfast_crime Dealt_Rate  data is stationary
adf.test(Dealt_Rate, alternative = "stationary")
#As from the plot, the data apperas to be non-stationary
#Since this is monthly data, we need to expand the test’s lag visibility to 12 so it can see the seasonality.
# The Dickey-Fuller value obtined is -2.3482
#The p-value is 0.4298, that is  again less than 5% (p <0.05),  so we reject the null hypothesis
adf.test(Dealt_Rate, alternative = "stationary", k = 12)


#Another way to test for stationarity is to use autocorrelation.
#The ACF of non-stationary data decreases slowly. 
#From this plot , it can be seen that the ACF is slowly dropping to zero, but there are fluctuations also
Acf(Dealt_Rate)
Pacf(Dealt_Rate)

#As the KPSS = not stationary and ADF = stationary -> it is having  difference stationary
#So using differencing to make series Strict Stationary
#A strict stationary series satisfies the mathematical definition of a stationary process. 
#For a strict stationary series, the mean, variance and covariance are not the function of time
kpss.test(Dealt_Rate)


#No seasonal differences are suggested as FS<0.64
#The output suggests that,  no log is required to make the data stationary.
nsdiffs(Dealt_Rate)
#The output suggests that, one differencing is required to make the data stationary
ndiffs(Dealt_Rate)

#From the plot and seasonal decomposition, it is evident that, there is seasonality in data
#So doing seasonal differencing on data 
#using diff() to stabilise the mean of the data
Dealt_Rate_d1 <- diff(Dealt_Rate, lag = 12, differences =1)

# Showing both differenced and original data side-by-side for comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(Dealt_Rate, main = "Original Belfast city- Criminal Damage dealt rate dataset")
plot(Dealt_Rate_d1, main = "Differenced Belfast city- Criminal Damage dealt rate datase")
par(opar)


#We need to examine whether there's a trend in the data.
# p-value < 0.05 indicates the TS is stationary
#The dicker-fuller value is -4.2771, but with lag=5
adf.test(Dealt_Rate_d1, alternative = "stationary")
# p-value is 0.1 which is greater than 0.05, that indicates the TS is stationary
kpss.test(Dealt_Rate_d1)


# Plotting the Acf and Pacf of stationary data to identify p and q values 
Acf(Dealt_Rate_d1)
Pacf(Dealt_Rate_d1)

#ARIMA MODEL
#Using auto-arima model to find the p, q and d values that fits the data 
#This model shows an AIC value of 1012.27 and BIC value of 1029.33
auto_arima <- auto.arima(Dealt_Rate)
auto_arima

#p – The lag value where the PACF shuts off at lag=2.
#q – There is an expoential decrease till 3 in ACF 
#This model shows an aic value of 969.05
#The ma is 81% and ar is 89%
arima(Dealt_Rate, c(2,1,3), seasonal = list(order = c(2,1,3), period = 12))

#p – The lag value where the PACF shuts off at lag=2, but it is below 0, so testing with lag = 1.
#q – There is an expoential decrease till 3 in ACF 
#This model shows an aic value of 977.58
#The ma3 is 74% and ar is 87%
arima(Dealt_Rate, c(1,1,3), seasonal = list(order = c(1,1,3), period = 12))

#p – The lag value where the PACF chart crosses the upper confidence interval for the first time. In this case p=1.
#q – The lag value where the ACF chart crosses the upper confidence interval for the first time. In this case q=1.
#This model shows an AIC value of 970.19
#The ma is 83% and ar is 11%
arima(Dealt_Rate, c(1,1,1), seasonal = list(order = c(1,1,1), period = 12))

#Of these models, the best is the ARIMA(2,1,3)(2,1,3)12 model (i.e.it has the smallest AIC value).
#Fitting the selected ARIMA model
fit <- arima(Dealt_Rate, c(2,1,3), seasonal = list(order = c(2,1,3), period = 12))
fit


#Predicting the crime dealing rate for 3 years
prediction <- predict(fit, n.ahead = 3 * 12)
prediction


#Plot a forecast of the time series using the forecast function
#with a 95% confidence interval where h is the forecast periods in months.
forecast_dealing_rate <- forecast(fit, level = c(95), h = 36)
forecast_dealing_rate

#Plotting the forecasted values
autoplot(forecast_dealing_rate)


#Comparisons
#auto_arima AIC = 1012.55
#fit AIC = 969.05

#Auto_arima coefficient for the moving averages is -0.6773
#fit coefficient for the moving averages is -0.8123

#Auto_arima MAPE = 17.51857
#fit MAPE = 14.48645
accuracy(auto_arima)
accuracy(fit)


plot(forecast(auto_arima, 3 * 12), xlab = "Year", ylab = "Criminal Damage cases delaing rate")
plot(forecast(forecast_dealing_rate, 3), xlab = "Year", ylab = "Criminal Damage cases delaing rate ")


#Evaluating the model
qqnorm(auto_arima$residuals)
qqline(auto_arima$residuals)

#The Box.test() function tests that the autocorrelations are all zero.
#H0 = the autocorrelations are all zero.
#p > 0.05, so fail to reject the null hypothesis. This ARIMA model appears to fit the data well
Box.test(auto_arima$residuals, type = "Ljung-Box")

#Applying the same tests to the manually selected model.
qqnorm(fit$residuals)
qqline(fit$residuals)
#The fit p value is slightly better than the auto ARIMA p value
#p-value = 0.9542
Box.test(fit$residuals, type = "Ljung-Box")


#Choosing training and testing data
#Assessing the predicted values versus actual values from the dataset
dealt_rate_train <- window(x = Dealt_Rate, start=c(2001,4), end=c(2016, 12))
dealt_rate_test <- window(x = Dealt_Rate, start=c(2017))

dealt_rate_train
dealt_rate_test


fit <- arima(dealt_rate_train, c(2,1,3), seasonal = list(order = c(2,1,3), period = 12))
fit

auto_arima <- auto.arima(dealt_rate_train)
auto_arima


predict_auto_ARIMA <- forecast(auto_arima, 3 * 12)
predict_auto_ARIMA


precict_manual_ARIMA <- forecast(fit, 3 * 12)
precict_manual_ARIMA



# make actuals_predicted dataframe
# for manual ARIMA
actuals_predictions <- data.frame(cbind(actuals = dealt_rate_test, predicted = precict_manual_ARIMA))
head(actuals_predictions)


correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy