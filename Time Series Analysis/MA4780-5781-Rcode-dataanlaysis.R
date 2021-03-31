# Airline Passengers dataset 
data(AirPassengers)

######
#  1. Plot the data. 
#     Look for any patterns - linear trend? random walk? seasonal patter? 
#	See if you need to transform the data,
######
plot(AirPassengers,main="Monthly totals of international airline passengers")
plot(diff(AirPassengers))
plot(diff(log(AirPassengers)))

# It seems that log transformation is appropriate.
lx=log(AirPassengers)
plot(lx)

######
#  2. If there is seasonal pattern,
#     consider seasonal means model or cosine trends model [CHAHPTER 3].
#     Ideally, fit both models, see which one fits better.
#     Usually, seasonal means model has a better chance.
#     If both are doing similarly well, use cosine trend model, which is more parsimonious.
#     And/or, optionally, a seasonal ARIMA model [CHAPTER 10] can be used.
#	If seasonal ARIMA model will be used, this step can be skipped.
######

# Here, let's try seasonal means model.
library(TSA)
month=season(lx)
model1=lm(lx~month)

lx1=ts(resid(model1),start=c(1949,1),freq=12)
plot(lx1)


######
#  3. If there seems to be a clear deterministic trend, remove the trend.
#     Popular choices are polinomial trends, including linear trend. [CHAPTER 3]
######

model2=lm(lx1~time(lx1))
summary(model2)
plot(model2)
lx2=ts(resid(model2),start=c(1949,1),freq=12)
plot(lx2)


#####	If desired, steps 2 and 3 can be done simulataneously
#####	putting seasonal trend and polinomial trend in the same model.
model3=lm(lx~month+time(lx))
plot(lx)
f.lx3=ts(fitted(model3),start=c(1949,1),freq=12)
lines(f.lx3,col=2,lty=2)
lx3=residuals(model3)


####
# Check the diagnostics of this deterministic trend model
# 1) Residual plot (zero mean and homoscedasticity)
plot(rstandard(model3))
abline(h=0,col=2)

#####
# It seems that there is nonlinear pattern in the residual plot.
# One option is to re-fit the model with a quadratic trend.
tm=time(lx)
tm2=time(lx)^2
#model4=lm(lx~month+time(lx)+I(time(lx)^2))
model4=lm(lx~month+tm+tm2)
plot(lx)
f.lx4=ts(fitted(model4),start=c(1949,1),freq=12)
lines(f.lx4,col=2,lty=2)
lx4=residuals(model4)


####
# Check the diagnostics of this deterministic trend model
# 1) Residual plot (zero mean and homoscedasticity)
plot(rstandard(model4))
abline(h=0,col=2)


# 2) QQ plot (normality)
qqnorm(lx4)
qqline(lx4)

# 3) Shapiro-Wilk test (normality) and runs test (independence)
shapiro.test(lx4)
runs(lx4)

# 4) ACF plot (independence)
acf(lx4)

## All assumptions seem fine, except for independence.
##### At this point, you might be interested in the slope of the linear trend or coefficient of the quadtratic trend.
##### If it is the case, you can perform a HAC test on the slope coefficient
##### for a more accurate p-value.
##### (See the lecture notes and R code for Chapter 3)
require(sandwich)  # for consistent variance estimation: use 'vcovHAC' function
require(lmtest)		# for testing hypothesis for coefficients: us 'coeftest' function
coeftest(model4, vcov=vcovHAC(model4))


#####
#   4. If you want to forecast, fit an arima model for the detrended data.
#   4-(1). Find the order of the ARIMA model [CHAHPTER 6]
#   4-(2). Fit the candidate models [CHAPTER 7]
#   4-(3). Choose the best model based on information criteria. [CHAPTER 6]
#   4-(4). Model diagnostics [CHAPTER 8]
#   4-(5). Try overfitting [CHAPTER 8]
#####


# d
adf.test(lx4)
pp.test(lx4)


# p & q
par(mfrow=c(1,2))
acf(lx4)    # MA(3)
pacf(lx4)   # AR(1)
par(mfrow=c(1,1))
eacf(lx4)   # MA(3), AR(1), ARMA(2,2)

library(forecast)
auto.arima(lx4)  # AR(1)

# Candidate models could be MA(3), AR(1), or ARMA(2,2)
ma3=Arima(lx4,order=c(0,0,3),include.mean=F)	#
ar1=Arima(lx4,order=c(1,0,0),include.mean=F)
arma22=Arima(lx4,order=c(2,0,2),include.mean=F)	#

# Compare the three models
# Based on AIC or AICc, choose MA(3).  [for  better forecasting]
# Based on BIC, choose AR(1).  [for identifying the right model]
ma3
ar1
arma22


# Model diagnostics
library(TSA)
tsdiag(ma3)
tsdiag(ar1)
tsdiag(arma22)


##### Let's try forecasting for the next three years 
##### based on the AR(1) model
newtm=seq(from=1961,to=1963.917,length=36)
newdata=data.frame(month=as.factor(month[1:36]),tm=newtm,tm2=newtm^2)
predxreg=predict(model4,newdata)

predx=predict(ar1,n.ahead=36)
pr=predx$pred+predxreg
uci=pr+2*predx$se
lci=pr-2*predx$se


# To plot the predicted values as prediction intervals, code them as time series
pr=ts(pr,start=1961,freq=12)
uci=ts(uci,start=1961,freq=12)
lci=ts(lci,start=1961,freq=12)

ymin=min(c(as.vector(lci),lx))-.1
ymax=max(c(as.vector(uci),lx))+.1

plot(lx,xlim=c(1949,1964),ylim=c(ymin,ymax),main="Log of Airline Passenger")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)


## In the original scale..
plot(exp(lx),ylab="International airline passengers (in thousands)",main="Airline Passenger",xlim=c(1949,1964),ylim=c(exp(ymin),exp(ymax)))
lines(exp(pr),col=2)
lines(exp(uci),col=3)
lines(exp(lci),col=3)



##### We can also build the model using the original data.
##### Use xreg option to add deterministic trend.
##### Coefficient estimations are going to be slightly different.
##### xreg option can also be used if you have covariates (x random variables in regression) in your model as well.
##### Recall that lx was the log transformed data.

ar1_xreg=Arima(lx,order=c(1,0,0),include.mean=F,xreg=model.matrix(model4))

# forecast the next three years
predx=predict(ar1_xreg,n.ahead=36,newxreg=cbind(1,rbind(diag(1,12)[,-1],diag(1,12)[,-1],diag(1,12)[,-1]),newdata[,2:3]))
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

# To plot the predicted values as prediction intervals, code them as time series
pr=ts(pr,start=1961,freq=12)
uci=ts(uci,start=1961,freq=12)
lci=ts(lci,start=1961,freq=12)

ymin=min(c(as.vector(lci),lx))-.1
ymax=max(c(as.vector(uci),lx))+.1


plot(lx,xlim=c(1949,1964),ylim=c(ymin,ymax),main="Log of Airline Passenger")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)

## In the original scale..
plot(exp(lx),ylab="International airline passengers (in thousands)",main="Airline Passenger",xlim=c(1949,1964),ylim=c(exp(ymin),exp(ymax)))
lines(exp(pr),col=2)
lines(exp(uci),col=3)
lines(exp(lci),col=3)


#####
#   5. If you want to use SARIMA model, 
# http://people.duke.edu/~rnau/seasarim.htm#_top
#####

model0=lm(lx~tm+tm2)
lx0=ts(resid(model0),start=c(1949,1),freq=12)
plot(lx0)

acf(lx0)
pacf(lx0)

model0_sarima=arima(lx0,order=c(1,0,0),seasonal=list(order=c(0,1,1)))
tsdiag(model0_sarima)

auto.arima(lx0)

# compare ICs for the candidate models
Arima(lx0,order=c(1,0,0),seasonal=list(order=c(0,1,1)))
Arima(lx0,order=c(2,0,0),seasonal=list(order=c(0,1,1)))
      