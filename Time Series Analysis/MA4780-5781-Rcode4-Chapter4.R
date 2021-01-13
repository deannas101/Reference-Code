## Population ACF and Sample ACF plots for MA and AR models
#### 1. Population ACF: You have two (or more) options in R.
####### 1) Using a prewritten function, ARMAacf.
####### 2) Write your own function using the formulas we calculated.
########## Option 1) is more convinient, but you need to be careful when you enter the coefficients.
#### 2. Sample ACF: use acf function in base package (with lag 0) OR acf function in TSA packages (without lag 0)


## Population ACF for an MA(1) model with theta=-0.9
y = ARMAacf(ma = 0.9, lag.max = 20)	
	# Notice that 0.9 is used instead of -0.9, because their MA model is defined as
	# Y_t = e_t + theta * e_{t-1}
plot(y, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an MA(1) model with coefficient -0.9")
abline(h=0)

## Sample ACF for an MA(1) model with theta=-0.9
# Generate an MA(1) process at random
set.seed(122)	# try changing this seed number to generate different y.
n = 100
e = rnorm(n+1)
theta = -0.9
y = ts(e[2:(n+1)]-theta*e[1:n])
acf(y,main="Sample ACF of an MA(1) model with coefficient -0.9")


###################
## Population ACF for an AR(2) model with phi_1=1, phi_2=-0.5
y = ARMAacf(ar = c(1,-0.5), lag.max = 20)	
	# Notice that we do not have to change sign for the AR model.
	# Y_t = phi_1 * Y_{t-1} + phi_2 * Y_{t-2} + e_t
plot(y, x = 0:20, type = "h", ylim = c(-1,1), xlab = "k", ylab = "Autocorrelation", main = "Population ACF of an AR(2) model with coefficients 1 and -0.5")
abline(h=0)

## The same population ACF can be obtained using the recursive algorithm on slide 40 for Chapter 4.
phi1 = 1; phi2 = -0.5; max.lag = 20
rho = rep(0,max.lag)
rho[1] = phi1/(1-phi2); rho[2] = (phi2*(1-phi2)+phi1^2)/(1-phi2)
for (k in 3:max.lag) rho[k]=phi1*rho[k-1]+phi2*rho[k-2]
plot(y=c(1,rho), x=0:max.lag, type='h', ylab='ACF', xlab='Lag', ylim=c(-1,+1), main = "Population ACF of an AR(2) model with coefficients 1 and -0.5")
abline(h=0)


## Sample ACF for an AR(2) model with phi_1=1, phi_2=-0.5
# Generate an AR(2) process at random
set.seed(2190)	# try changing this seed number to generate different y.
n = 100
e = rnorm(n+1)
phi1 = 1
phi2 = -0.5
y = rep(0,n); y[1] = e[1]; y[2] = e[2]
for (i in 3:n){ y[i] = phi1*y[i-1]+phi2*y[i-2]+e[i]}
acf(y,main="Sample ACF of an AR(2) model with coefficients 1 and -0.5")
