#### MoM Estimator for MA(1)

estimate.ma1.mom=function(x){
	  require(TSA)
      r=acf(x,plot=F)$acf[1]; if (abs(r)<0.5) return((-1+sqrt(1-4*r^2))/(2*r))
      else return(NA)
      }

set.seed(160401)
n=100	 # Sample size of each MC simulation
x=arima.sim(list(order = c(0,0,1), ma = c(-0.7)), n = n)
estimate.ma1.mom(x)	# If you repeat, you have NA estimates for some x.


#### Repeat to see what proportion of MoM estimates do not exist.
set.seed(160402)
M=2000	 # number of Monte-Carlo simulations
n=100	 # Sample size of each MC simulation

thetaMoM=rep(0,M)	#estimates will be stored here
for (i in 1:M){
	x=arima.sim(list(order = c(0,0,1), ma = c(-0.7)), n = 100)	# This is your observation
	thetaMoM[i]=estimate.ma1.mom(x)
	}
mean(is.na(thetaMoM))	# about 30% of the data, you cannot calculate the MoM estimator!
hist(thetaMoM)
mean(thetaMoM,na.rm=T)	# center of the sampling distribution of the MoM estimator	
						# The true parameter is 0.7 (not -0.7: recall that the signs of the MA model in our book and arima function is opposite!)




#### Sampling distribution of CSS and ML estimators for MA(1)
set.seed(160402)
M=2000	 # number of Monte-Carlo simulations
n=100	 # Sample size of each MC simulation

thetaCSS=thetaML=rep(0,M)	#estimates will be stored here
for (i in 1:M){
	x=arima.sim(list(order = c(0,0,1), ma = c(-0.7)), n = 100)	# This is your observation
	thetaCSS[i]=arima(x,order=c(0,0,1),method="CSS")$coef[1]
	thetaML[i]=arima(x,order=c(0,0,1),method="ML")$coef[1]
	}

# Historgram of the sampling distributions of MoM, CSS, and ML estimators
par(mfrow=c(1,3))
hist(thetaMoM,main="MoM")
hist(-thetaCSS,main="CSS")
hist(-thetaML,main="ML")
par(mfrow=c(1,1))

# Center of the sampling distributios of MoM, CSS, and ML estimators
mean(thetaMoM,na.rm=T)
mean(-thetaCSS)
mean(-thetaML)

# Standard deviation of the sampling distributios of MoM, CSS, and ML estimators
sd(thetaMoM,na.rm=T)
sd(-thetaCSS)
sd(-thetaML)


### confidence interval of MLE for MA(1)
set.seed(15341)
x=arima.sim(list(order = c(0,0,1), ma = c(-0.7)), n = 100)
mlx=arima(x,order=c(0,0,1),method="ML")
mlx		# s.e. from here is more generic

thetahat=mlx$coef[1]
se=sqrt((1-thetahat^2)/n)
ciu=thetahat+2*se	# upper limit of 95% confidence interval
cil=thetahat-2*se	# lower limit of 95% confidence interval
thetahat
se					# This se is talored for the MA(1) model
ciu
cil



## Real GNP example.
setwd("/Users/yrho-adm/Google Drive/2-1.Teach/2016Spring_MA4780-5781/5.Rcode")
GNP=read.csv("ch6-realGNP-original.csv",head=F)$V1
GNP=ts(GNP,start=c(1947,1),freq=4)

newGNP=diff(log(GNP))
ar.yw(newGNP,order.max=1)	# This code is the same as
ar(newGNP,method="yw",order.max=1)
ar.ols(newGNP,order.max=1)	# This code is the same as
ar(newGNP,method="ols",order.max=1)
ar.mle(newGNP,order.max=1)	# This code is the same as
ar(newGNP,method="mle",order.max=1)


# Hare data
library(TSA)
data(hare)
ar.yw(hare,order.max=2,demean=T,intercept=F)
ar.ols(hare,order.max=2,demean=T,intercept=F)
ar.mle(hare,order.max=2,demean=T,intercept=F)

arima(hare,order=c(2,0,0),method="CSS")
arima(hare,order=c(2,0,0),method="ML")
arima(hare,order=c(2,0,0),method="CSS-ML")	# CSS-ML option is the default. 

library(forecast)
Arima(hare,order=c(2,0,0),method="CSS")
Arima(hare,order=c(2,0,0),method="ML")
Arima(hare,order=c(2,0,0),method="CSS-ML")	# CSS-ML option is the default. 

# One thing that you want to be careful about using functions arima and Arima in R is that
# the 'intercept' is actually the grand mean. 
# compare the followings.
ar.yw(hare,order.max=2,demean=T,intercept=F)$x.mean
mean(hare)
arima(hare,order=c(2,0,0),method="CSS")$coef[3]
Arima(hare,order=c(2,0,0),method="CSS")$coef[3]
arima(hare,order=c(2,0,0),method="ML")$coef[3]
Arima(hare,order=c(2,0,0),method="ML")$coef[3]
