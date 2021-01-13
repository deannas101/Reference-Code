
#########
# Asymptotic standard deviation of a sample ACF for an AR(1) process
k=1:30
phi=0.9
n=100
srkar=((1+phi^2)*(1-phi^(2*k))/(1-phi^2)-2*k*phi^(2*k))/n
asdar=round(sqrt(srkar),2)
asdwn=1/sqrt(n)
cbind(k,asdar,asdwn)  # column1: lag
# column2: asymptotic standard deviation of rk from an AR(1) process
# column3: asymptotic standard deviation of rk from an iid process

# Asymptotic standard deviation of a sample ACF for an MA(1) process
theta=0.9
n=100
rho1=-theta/(1+theta^2)
srkma=c((1-3*rho1^2+4*rho1^4)/n,(1+2*rho1^2)/n)
asdma=round(sqrt(srkma),2)
asdwn=1/sqrt(n)
cbind(1:2,asdma,asdwn)	# column1: lag
# column2: asymptotic standard deviation of rk from an MA(1) process
# column3: asymptotic standard deviation of rk from an iid process



###### Examples of sample ACF and PACF plots for AR and MA processes
## 1. Four simulated MA(1) series with MA coefficient -0.8
par(mfcol=c(2,4))
for (i in 1:4){ 
  x=arima.sim(n=100,list(order=c(0,0,1),ma=0.8))
  acf(x,main="sample ACF")
  pacf(x,main="sample PACF")
}
par(mfrow=c(1,1))

## 2. Four simulated MA(2) series with MA coefficients -0.8 and -0.7
# This is MA process is invertible since 1+0.8z+0.7z^2 has roots outside of a unit circle.

polyroot(c(1,0.8,0.7))
Mod(polyroot(c(1,0.8,0.7)))

par(mfcol=c(2,4))
for (i in 1:4){ 
  x=arima.sim(n=100,list(order=c(0,0,2),ma=c(0.8,0.7)))
  acf(x,main="sample ACF")
  pacf(x,main="sample PACF")
}
par(mfrow=c(1,1))


## 3. Four simulated MA(3) series with MA coefficients -0.8, -0.7, and -0.6
# This is MA process is invertible since 1+0.8z+0.7z^2+0.6z^3 has roots outside of a unit circle.
polyroot(c(1,0.8,0.7,0.6))
Mod(polyroot(c(1,0.8,0.7,0.6)))

par(mfcol=c(2,4))
for (i in 1:4){ 
  x=arima.sim(n=100,list(order=c(0,0,3),ma=c(0.8,0.7,0.6)))
  acf(x,main="sample ACF")
  pacf(x,main="sample PACF")
}
par(mfrow=c(1,1))


## 4. Four simulated AR(1) series with AR coefficient -0.8 
par(mfcol=c(2,4))
for (i in 1:4){ 
  x=arima.sim(n=100,list(order=c(1,0,0),ar=-0.8))
  acf(x,main="sample ACF")
  pacf(x,main="sample PACF")
}
par(mfrow=c(1,1))


## 5. Four simulated AR(2) series with AR coefficient -0.8 and -0.7
par(mfcol=c(2,4))
for (i in 1:4){ 
  x=arima.sim(n=100,list(order=c(2,0,0),ar=c(-0.8,-0.7)))
  acf(x,main="sample ACF")
  pacf(x,main="sample PACF")
}
par(mfrow=c(1,1))


## 6. Four simulated AR(3) series with AR coefficient -0.8, -0.7, and -0.6
par(mfcol=c(2,4))
for (i in 1:4){ 
  x=arima.sim(n=100,list(order=c(3,0,0),ar=c(-0.8,-0.7,-0.6)))
  acf(x,main="sample ACF")
  pacf(x,main="sample PACF")
}
par(mfrow=c(1,1))
