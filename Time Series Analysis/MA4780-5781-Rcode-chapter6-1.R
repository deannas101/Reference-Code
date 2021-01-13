## Determining the order of integration d
## Real GNP example.
setwd("~/0.teaching/2018Spring_MA4780-5781/5.Rcode")
GNP=read.csv("ch6-realGNP-original.csv",head=F)$V1
GNP=ts(GNP,start=c(1947,1),freq=4)
plot(GNP)
acf(GNP)

# unit root tests
library(tseries)	# both adf.test and pp.test functions are implemented in this 'tseries' package.
adf.test(GNP)	
pp.test(GNP)

# The p-value>0.05 -> do not reject the unit root null hypothesis -> real GNP series is a random walk.
# Take difference to remove the nonstationarity.
plot(diff(GNP))
acf(diff(GNP))
adf.test(diff(GNP))
pp.test(diff(GNP))

# It seems that the variability increases over time.
# For many nonstationary macroeconomic variables, it is common to take log difference, or equivalently, percentage change.
# Tip: If you are going to take log transformation and take difference anyway,
#      take log transformation before taking difference.
#      i.e., choose diff(log(GNP)) rather than log(diff(GNP))
#      This is to avoid taking log for negative values, and has better interpretation.
newGNP=diff(log(GNP)) 
# Don't do log(diff(GNP))
plot(newGNP)
acf(newGNP)
adf.test(newGNP)
pp.test(newGNP)



###### Also, consider detrending ######
## Since it is reasonable to consider a linear trend in the GNP and log(GNP) plots, 
## Let's try detrending this deterministic trend first.

# Step 1. Fit a linear trend model
model1=lm(GNP~time(GNP))
summary(model1)	# Keep in mind that these p-values may not be reliable.

# Step 2. Use the residual from the linear trend model as your time series.
#    Use the usual procedures to determine d (time plot, acf plot, unit root tests). 
y=ts(resid(model1),start=c(1947,1),freq=4)
plot(y)
acf(y)
adf.test(y)
pp.test(y)

plot(diff(y))
acf(diff(y))
adf.test(diff(y))
pp.test(diff(y))

# Step 3 (optional): If you have observed that the variability increases in plot(diff(y)),
# 	    consider log transformation.
# Start from the linear trend estimation.
model2=lm(log(GNP)~time(GNP))
summary(model2)	# Keep in mind that these p-values may not be reliable.

y2=ts(resid(model2),start=c(1947,1),freq=4)
plot(y2)
acf(y2)
adf.test(y2)
pp.test(y2)

plot(diff(y2))
acf(diff(y2))
adf.test(diff(y2))
pp.test(diff(y2))


## Keep in mind that there is NO RIGHT MODEL when it comes to the real data analysis.
## We can talk about a better model, though.
## For this GNP example, it seems pretty clear that d=1.
## On top of that, I would say the model with the linear trend and the log transform is better than the ones without them.
## To come to this decision, I would look at different aspects of this data set.

### Look at the graphs.
# It seems that the linear trend is quite helpful.
# There seems to be a exponental growth pattern in GNP and the variance increses over time in differenced detrended GNP.
par(mfrow=c(3,2))
plot(GNP,main="GNP")
abline(model1,lty=2)
plot(log(GNP),main="log(GNP)")
abline(model2,lty=2)
plot(y,main="Detrended GNP")
plot(y2,main="Detrended log(GNP)")
plot(diff(y),main="Difference of Detrended GNP")
plot(diff(y2),main="Difference of Detrended log(GNP)")
par(mfrow=c(1,1))

### Also, R^2 is better with log transformation.
summary(model1)
summary(model2)


### Now, let's think about the difference between the models with and without detrending.
### In the end, the nonstationarity will be removed without detrending, too.

par(mfrow=c(2,2))
plot(diff(y),main="Difference of Detrended GNP",ylim=c(-500,250))
abline(h=0)
abline(h=mean(diff(y)),lty=2,col=2)
plot(diff(GNP),main="Difference of GNP",ylim=c(-500,250))
abline(h=0)
abline(h=mean(diff(GNP)),lty=2,col=2)
plot(diff(y2),main="Difference of Detrended log(GNP)",ylim=c(-0.04,0.04))
abline(h=0)
abline(h=mean(diff(y2)),lty=2,col=2)
plot(diff(log(GNP)),main="Difference of log(GNP)",ylim=c(-0.04,0.04))
abline(h=0)
abline(h=mean(diff(log(GNP))),lty=2,col=2)
par(mfrow=c(1,1))


## Q: Which one looks better? 
## A: The detrended one is better becuase it produces a mean zero time series. 
##	Thus subsequent analysis (deciding p and q of ARMA(p,q) model) makes better sense,
##	since we consdier 'mean zero' ARMA(p,q) model.
## 	Detrending also give you a better understanding of the data.
##	In this case, we can say that the GNP and log(GNP) are random walk process with drift.
##	It means that the GNP and log(GNP) can be understood as random walks around a linear trend.
##	On the other hand, if you did not consider detrending in the first place, 
## 	You cannot distinguish between 'random walk' and 'random walk with drift.'


## Whenever you are not confident on which model to choose, proceed with several (reasonable) candidate models.
## In the end, you can use information criteria (AIC, AICc, and BIC) to choose the final model.


##############
## Repeat the same exercise with oil.price dataset in TSA package.
## The same example appears in the end of this R code so that you can practice how to choose all parameters.
library(TSA)
data(oil.price)

plot(oil.price)
acf(oil.price)
adf.test(oil.price)
pp.test(oil.price)

# First difference
plot(diff(oil.price))
acf(diff(oil.price))
adf.test(diff(oil.price))
pp.test(diff(oil.price))

# Log difference
plot(diff(log(oil.price)))
acf(diff(log(oil.price)))
adf.test(diff(log(oil.price)))
pp.test(diff(log(oil.price)))



#######
## How to use the sample ACF and PACF plots with real data.
## Real GNP data: percent change from preceding period, quarterly, seasonally adjusted.
## US. Bureau of Economic Analysis, Real Gross National Product [A001RL1Q225SBEA], retrieved from FRED, Federal Reserve Bank of St. Louis https://research.stlouisfed.org/fred2/series/A001RL1Q225SBEA, March 14, 2016.
setwd("~/0.teaching/2018Spring_MA4780-5781/5.Rcode")
newGNP=read.csv("ch6-realGNPdata.csv",head=F)$V1
newGNP=ts(newGNP,start=c(1947,2),freq=4)
plot(newGNP)

# The acf plot in stats package (default in R) produces the acf plot with lag 0
# whereas the pacf plot in stats package produces the pacf plot without lag 0.
# The acf plot in TSA package, on the other hand, produce the acf plot without lag 0
# Load the TSA package to make the two plots consistent.

library(TSA)	
par(mfrow=c(1,2))
acf(newGNP,main="Sample ACF")
pacf(newGNP,main="Sample PACF")
par(mfrow=c(1,1))

eacf(newGNP)

# Based on the ACf and PACF plots, MA(2) and AR(1) seem to be reasonable candidates.


###############
## Information criteria

## 1. AIC using the arima function
# If you want to use the 'arima' function in 'stats' package unload TSA package.
# detach("package:TSA",unload=T)	# unload the previously loaded "TSA" package.
gnp.ar1=arima(newGNP, method="ML", order=c(1,0,0))
gnp.ma2=arima(newGNP, method="ML", order=c(0,0,2))
gnp.ar1$aic
gnp.ma2$aic	# MA(2) is chosen based on AIC.
			
## 2. Using the 'armasubsets' function in TSA package
## This function searches all ARMA(p,q) models from p=0,...,nar and q=0,...,nma.
## check ?plot.armasubsets
library(TSA)
res=armasubsets(y=newGNP,nar=14,nma=14,y.name='test',ar.method='ols')
plot(res)	# default is BIC
			# AR(1) model is chosen based on BIC
plot(res,scale='AIC')
plot(res,scale='AICc')	# more complicated models are suggested from AIC and AICc criterions.

## 3. Using 'Arima' function in 'forecast' package.
# install.packages("forecast")	# install forecast package when first using it.
library(forecast)
gnp.Ar1=Arima(newGNP, method="ML", order=c(1,0,0))
gnp.Ma2=Arima(newGNP, method="ML", order=c(0,0,2))
gnp.Ar1
gnp.Ma2	

## 4. Using 'auto.arima' function in 'forecast' package.
# Use with caution!
auto.arima(newGNP)




############
# Some real data examples - Section 6.6
## (1) Canadian Hare 
library(TSA)
data(hare)
par(mfrow=c(1,2))
plot(hare,type='b',main="Annual abundance of hare of the Hudson Bay in Canada")
plot(sqrt(hare),type='b',main="Annual abundance of hare of the Hudson Bay in Canada")
par(mfrow=c(1,1))

# Correlation plots to obtain some candidate models
par(mfrow=c(1,2))
acf(hare)
pacf(hare)
par(mfrow=c(1,2))
eacf(hare,ar.max=5,ma.max=5)

# armasubsets function in TSA package.
plot(armasubsets(y=hare,nar=5,nma=5,y.name='test',ar.method='ols'),scale="BIC")

# Compare AIC, corrected AIC, and BICs of AR(2) and AR(3) models
# We added AR(3) just to make comparisons
Arima(hare,method="ML",c(2,0,0))
Arima(hare,method="ML",c(3,0,0))

# Automatically select the orders
auto.arima(hare)



## (2) Oil Price
data(oil.price)
plot(oil.price,main="Monthly Oil Price")

dop=diff(oil.price)
dlop=diff(log(oil.price))
par(mfrow=c(1,2))
plot(dop,main="Differenced Series of Monthly Oil Price")
plot(dlop,main="Log difference of Monthly Oil Price")
par(mfrow=c(1,1))


# Unit root test to determine d
adf.test(oil.price);pp.test(oil.price)
adf.test(log(oil.price)); pp.test(log(oil.price))

adf.test(dop); pp.test(dop)
adf.test(dlop); pp.test(dlop)

#### Determining the orders of p and q
# Correlation plots to obtain some candidate models
par(mfrow=c(1,2))
acf(dlop)
pacf(dlop)
par(mfrow=c(1,1))
eacf(dlop,ar.max=5,ma.max=5)

# armasubsets function in TSA package.
plot(armasubsets(y=dlop,nar=5,nma=5,y.name='test',ar.method='ols'),scale="BIC")

# Compare AIC, corrected AIC, and BICs of MA(1) and AR(2) models
Arima(dlop,method="ML",c(0,0,1))	#MA(1)
Arima(dlop,method="ML",c(2,0,0))	#AR(2)

# The 'intercept' of MA(1) and AR(2) are not significantly differnt from zero.
# To remove the intercept term, do the follows. 
# Removing intercept terms give slightly better AIC, AICc, and BIC.
Arima(dlop,method="ML",c(0,0,1),include.mean=FALSE)
Arima(dlop,method="ML",c(2,0,0),include.mean=FALSE)

# Automatically select the orders
auto.arima(dlop)


### Note:
# The way AIC is calculated is slightly different in Arima and arima functions.
# Arima function produces AICc and BIC as well.
# Suggestion: use Arima function, rather than arima.
