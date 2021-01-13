########################
## 1. Overfitting
########################
library(forecast)
library(TSA)

x=arima.sim(n=100,list(order=c(1,0,0),ar=0.8))
plot(x)
acf(x)
pacf(x)
ar1=Arima(x,order=c(1,0,0))


# overfitting
ar2=Arima(x,order=c(2,0,0))
arma11=Arima(x,order=c(1,0,1))

# compare coefficient estimates and information criteria
ar1
ar2
arma11


#####
# How to test if the additional AR or MA parameters are significant.
#####
phi2=ar2$coef[2]
sephi2=sqrt(diag(ar2$var.coef))[2]
phi2+2*sephi2	# upper limit of the 95% CI for phi2
phi2-2*sephi2	# lower limit of the 95% CI for phi2
			# Since the 95% CI does include 0, phi2 is not significantly different from zero.

theta1=arma11$coef[2]
setheta1=sqrt(diag(arma11$var.coef))[2]
theta1+2*setheta1	# upper limit of the 95% CI for theta1
theta1-2*setheta1	# lower limit of the 95% CI for theta1
			# Since the 95% CI does include 0, theta1 is not significantly different from zero.


########################
# 2. Diagnostic plots for the chosen model, AR(1)
########################
tsdiag(ar1)
