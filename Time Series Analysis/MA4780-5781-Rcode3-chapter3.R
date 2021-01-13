## Chapter 3

##Generate a moving average process
set.seed(12345)
n=100
e <- rnorm(n+1)
#theta=0.5
theta=-0.5
y <- ts(e[2:(n+1)]+theta*e[1:n])
plot(y,type='o',ylab="",xlab="Time",main="A simulated MA process with MA coefficient 0.8",ylim=c(-4,4))
abline(h=0,lty=2)

#############################
## 1.  test if the mean is zero
############################# 
#### (1) usual t test	# the p-values are NOT appropriate for our data.
fm=lm(y~1)
summary(fm)

#### (2) HAC test	# the p-values are appropriate for our data.
require(sandwich)	# for consistent variance estimation: use 'vcovHAC' function
require(lmtest)		# for testing hypothesis for coefficients: us 'coeftest' function
coeftest(fm, vcov=vcovHAC(fm))

#### plot the data and the fitted constant trend function
plot(y,type='o',ylab="",xlab="Time",main="A simulated MA process with MA coefficient 0.8",ylim=c(-4,4))
abline(h=coef(fm),lty=2)

#### compare the standard error in (1) and (2)
## (1) standard error in 'lm' function
se1=summary(fm)$coeff[1,2]

## (2) standard error from HAC estimation
se2=coeftest(fm, vcov=vcovHAC(fm))[1,2]

## Look at their ratio.
## Theoretical value for (se2/se1)^2 is 1.8 if theta=0.5, 0.2 if theta=-0.5
(se2/se1)^2


###########################
## 2.  test if there is a linear trend 
###########################


#### (1) usual t test	# the p-values are NOT appropriate for our data.
fm1=lm(y~time(y))
summary(fm1)

#### (2) HAC test	# the p-values are appropriate for our data.
require(sandwich)	# for consistent variance estimation: use 'vcovHAC' function
require(lmtest)		# for testing hypothesis for coefficients: us 'coeftest' function
coeftest(fm1, vcov=vcovHAC(fm1))

#### plot the data and the fitted linear trend function
plot(y,type='o',ylab="",xlab="Time",main="A simulated MA process with MA coefficient 0.8",ylim=c(-4,4))
abline(a=coef(fm1)[1],b=coef(fm1)[2],lty=2)

###########################
## 3. test if there is a quadratic trend 
###########################
t=time(y)
t2=t^2
fm2=lm(y~t+t2)
summary(fm2)

#### plot the data and the fitted quadratic trend function
plot(y,type='o',ylab="",xlab="Time",main="A simulated MA process with MA coefficient 0.8",ylim=c(-4,4))
curve(expr = coef(fm2)[1]+coef(fm2)[2]*x+coef(fm2)[3]*x^2,lty=2,add = TRUE)

###########################
## 4. Global Temperature data
###########################
library(astsa)
plot(gtemp, type="o", ylab="Temperature Anomalies (in Celsius)",main="Global Warming")
ftemp=lm(gtemp~time(gtemp))
abline(ftemp)

#### (1) usual t test	# the p-values are NOT appropriate for our data.
ftemp=lm(gtemp~time(gtemp))
summary(ftemp)

#### (2) HAC test	# the p-values are appropriate for our data.
require(sandwich)	# for consistent variance estimation: use 'vcovHAC' function
require(lmtest)		# for testing hypothesis for coefficients: us 'coeftest' function
coeftest(ftemp, vcov=vcovHAC(ftemp))


###########################
## 5. Seasonal Means model (tempdub data from TSA package)
###########################
library(TSA)
data(tempdub)
plot(tempdub, type="l", ylab="Temperature", xlab="Year", main="Average Monthly Temperature in Dubuque, Iowa, 1/1964-12/1975")
points(y=tempdub, x=time(tempdub), pch=as.vector(season(tempdub)) )
month = season(tempdub)
model1 =  lm(tempdub ~ month - 1)	# fit without intercept
summary(model1)

## compare the result with intercept
model2 =  lm(tempdub ~ month)	# fit without intercept
summary(model2)


###########################
## 6. Cosine Trends (tempdub data from TSA package)
###########################
har=harmonic(tempdub,1)
model3 = lm(tempdub ~ har)
summary(model3)


###########################
## Compare the fitted trends with Seaosnal Means model and Cosine Trends (tempdub data in TSA package)
###########################
plot(tempdub, type="l", ylab="Temperature", xlab="Year", main="Average Monthly Temperature in Dubuque, Iowa, 1/1964-12/1975")
fit1 = ts(fitted(model1),freq=12,start=c(1964,1))
lines(fit1, lty=2, col="red")
fit3 = ts(fitted(model3),freq=12,start=c(1964,1))
lines(fit3, lty=3, col="blue")


############################
## 7. Nonparametric Trends (bricks.txt data)
###########################
brick = read.table("~/Google Drive/2-1.Teach/2016Spring_MA4780-5781/5.Rcode/brick.txt", quote="\"")
br = ts(brick$V1, freq=4, start=1956)
plot(br, type="o", ylab="Brick Production (in millions)", xlab="Year", main="Average Quarterly Austrialian Clay Brick Production from 1956-1994")
lines(lowess(time(br),br),lty=2,lwd=2,col='red')			# default span (bandwidth) f is 2/3
lines(lowess(time(br),br,f=0.1),lty=2,lwd=2,col='blue')	# using reduced span of 0.1


############################
## 8. Residual Analysis
###########################

#############################
## 8-(1) Simulated MA process with constant mean model
#############################


set.seed(12345)
n=100
e <- rnorm(n+1)
#theta=0.5
theta=-0.5
y <- ts(e[2:(n+1)]+theta*e[1:n])
fm=lm(y~1)

# Residual plot
resid1=resid(fm)
plot(resid1,ylab="Residuals",xlab="Time",type="o",main="Residuals from an MA process with constant mean model")
abline(h=0,lty=2)

# Histogram of residuals
hist(resid1,main="Residuals from an MA process with constant mean model")

# QQ plot of residuals
qqnorm(resid1,main="QQ plot for the residuals from from an MA process with constant mean model")
qqline(resid1)

# Shapiro-Wilk test for normality (H_0: normal)
shapiro.test(resid1)

# Runs test (in TSA package) for independence (H_0: independent)
library(TSA)
runs(resid1)

# Sample ACF plot
acf(resid1,"Sample ACF plot for the residuals from from an MA process with constant mean model")

#########################
## 8-(2) Global Temperature data with linear trend
#########################
library(astsa)
ftemp=lm(gtemp~time(gtemp))

# Residual plot
resid2=resid(ftemp)
plot(resid2,ylab="Residuals",xlab="Time",type="o",main="Residuals from Global Temperature with linear trend model")
abline(h=0,lty=2)

# Histogram of residuals
hist(resid2,main="Residuals from Global Temperature with linear trend model")

# QQ plot of residuals
qqnorm(resid2,main="QQ plot for the residuals from Global Temperature with linear trend model")
qqline(resid2)

# Shapiro-Wilk test for normality (H_0: normal)
shapiro.test(resid2)

# Runs test (in TSA package) for independence (H_0: independent)
library(TSA)
runs(resid2)

# Sample ACF plot
acf(resid2,"Sample ACF plot for the residuals from Global Temperature with linear trend model")


#########################
## 8-(3) Temperature of Dubuque with Seasonal Means Model
#########################
library(TSA)
data(tempdub)
month = season(tempdub)
model1 =  lm(tempdub ~ month - 1)	# fit without intercept

# Residual plot
resid3=resid(model1)
plot(resid3,ylab="Residuals",xlab="Time",type="o",main="Residuals from Dubuque Temperature with Seasonal Means model")
abline(h=0,lty=2)

# Histogram of residuals
hist(resid3,main="Residuals from Dubuque Temperature with Seasonal Means model")

# QQ plot of residuals
qqnorm(resid3,main="QQ plot for the residuals from Dubuque Temperature with Seasonal Means model")
qqline(resid3)

# Shapiro-Wilk test for normality (H_0: normal)
shapiro.test(resid3)

# Runs test (in TSA package) for independence (H_0: independent)
library(TSA)
runs(resid3)

# Sample ACF plot
acf(resid3,"Sample ACF plot for the residuals from Dubuque Temperature with Seasonal Means model")
