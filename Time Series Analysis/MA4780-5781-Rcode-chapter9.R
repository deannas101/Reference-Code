# Chapter 9 - Forecasting
# Some examples of forecasting
library(TSA)


# 1. white noise series
set.seed(9876)
x=ts(rnorm(100))
plot(x)

par(mfrow=c(1,2))
acf(x)
pacf(x)
par(mfrow=c(1,1))

modelx=arima(x,order=c(0,0,0))
predx=predict(modelx,n.ahead=50)
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1


plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="White Noise (iid)")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)




# 2. AR(1)
set.seed(9876)
x=arima.sim(model=list(order=c(1,0,0),ar=0.8),n=100)
plot(x)

par(mfrow=c(1,2))
acf(x)
pacf(x)
par(mfrow=c(1,1))

modelx=arima(x,order=c(1,0,0))
predx=predict(modelx,n.ahead=50)
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1


plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="AR(1)")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)



# 3. ARMA(1,1)
set.seed(9876)
x=arima.sim(model=list(order=c(1,0,1),ar=0.8,ma=0.5),n=100)
plot(x)

par(mfrow=c(1,2))
acf(x)
pacf(x)
par(mfrow=c(1,1))
eacf(x)

modelx=arima(x,order=c(1,0,1))
predx=predict(modelx,n.ahead=50)
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1


plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="ARMA(1,1)")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)


# 4. Random walk
set.seed(9876)
x=ts(cumsum(rnorm(100)))
plot(x)

par(mfrow=c(1,2))
acf(x)
pacf(x)
par(mfrow=c(1,1))

modelx=arima(x,order=c(0,1,0))
predx=predict(modelx,n.ahead=50)
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1

plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="Random Walk")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)


# 5. Random walk with drift
set.seed(9876)
x=ts(cumsum(rnorm(100)+1))
plot(x)

par(mfrow=c(1,2))
acf(x)
pacf(x)
par(mfrow=c(1,1))

adf.test(resid(lm(x~time(x))))
pp.test(resid(lm(x~time(x))))

modelx=arima(x,order=c(0,1,0),xreg=1:length(x))
predx=predict(modelx,n.ahead=50,newxreg=length(x)+1:50)
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1


plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="Random Walk with Drift")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)


# 6. Deterministic linear trend with AR(1) errors
# Y_t = b_0 + b_1*t + X_t
# X_t ~ AR(1) with phi=0.7

set.seed(9876)
x0=arima.sim(model=list(order=c(1,0,0),ar=0.8),n=100)
x=0.5*time(x0)+x0
plot(x)

par(mfrow=c(1,2))
acf(resid(lm(x~time(x))))
pacf(resid(lm(x~time(x))))
par(mfrow=c(1,1))

adf.test(resid(lm(x~time(x))))
pp.test(resid(lm(x~time(x))))

modelx=arima(x,order=c(1,0,0),xreg=1:length(x))
predx=predict(modelx,n.ahead=50,newxreg=length(x)+1:50)
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1

plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="Deterministic Linear Trend with AR(1) errors")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)

# 7. Deterministic quadratic trend with AR(1) errors
# Y_t = b_0 + b_1*t + b_2*t^2 + X_t
# X_t ~ AR(1) with phi=0.7

set.seed(9876)
x0=arima.sim(model=list(order=c(1,0,0),ar=0.8),n=100)
x=10*((time(x0)-40)/50)^2+x0
plot(x)

par(mfrow=c(1,2))
acf(resid(lm(x~time(x)+I(time(x)^2))))
pacf(resid(lm(x~time(x)+I(time(x)^2))))
par(mfrow=c(1,1))

modelx=arima(x,order=c(1,0,0),xreg=cbind(time(x),time(x)^2))
predx=predict(modelx,n.ahead=50,newxreg=cbind(length(x)+1:50,(length(x)+1:50)^2))
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1

plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="Deterministic Quadratic Trend with AR(1) errors")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)


## If we have fitted a cubic function, the predictions may be very different.
## Carefully specifiy the deterministic trend functions!
modelx=arima(x,order=c(1,0,0),xreg=cbind(time(x),time(x)^2,time(x)^3))
predx=predict(modelx,n.ahead=50,newxreg=cbind(length(x)+1:50,(length(x)+1:50)^2,(length(x)+1:50)^3))
pr=predx$pred
uci=pr+2*predx$se
lci=pr-2*predx$se

ymin=min(c(as.vector(lci),x))-1
ymax=max(c(as.vector(uci),x))+1

plot(x,xlim=c(0,150),ylim=c(ymin,ymax),main="Deterministic Quadratic Trend with AR(1) errors - Cubic fit")
lines(pr,col=2)
lines(uci,col=3)
lines(lci,col=3)

