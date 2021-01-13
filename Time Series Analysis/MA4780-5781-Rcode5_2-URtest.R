## This R code is to help you better understand interpreting the unit root tests.

# 1. Understanding Unit root tests with simulated data

## I(1) series, y
x <- rnorm(50)  # No unit root (stationary)
y <- ts(cumsum(x)) # random walk

# To determin the order of integration, d, 
# Check time plot, ACF plot, and Unit root tests

# i) Time plot
plot(y)

# ii) ACF plot
acf(y)

# iii) Unit root tests
adf.test(y)
pp.test(y)

## Since we could not reject the unit root null hypothesis, take difference.
## Check time plot, ACF plot, and unit root tests to see if the difference series is stationary.
plot(diff(y))
acf(diff(y))
adf.test(diff(y))
pp.test(diff(y))

# This looks stationary.
# So we can say that y is I(1).

### An example of I(2) series
## I(2) series, z
z <- ts(cumsum(y)) 
plot(z)
acf(z)
adf.test(z)
pp.test(z)

# Take difference and check if stationary
plot(diff(z))
acf(diff(z))
adf.test(diff(z))
pp.test(diff(z))

# Does not seem to be stationary.
# Take another difference.
plot(diff(diff(z)))
acf(diff(diff(z)))
adf.test(diff(diff(z)))
pp.test(diff(diff(z)))

# This looks stationary. 
# Thus we can say that z is I(2).

# 2. Monte Carlo simuation of unit root tests
## Here, we are doing the so-called Monte-Carlo Simulation.
## Keep in mind that this type of experiment is not feasible in practice!!!

# sample size: 50
# MC replication: 2000 (In practice we only have 1 replication!)
pval=matrix(0,2000,4)
for (i in 1:2000){
  x <- rnorm(50)  # no unit-root
  pval[i,1]=adf.test(x)$p.value
  pval[i,2]=pp.test(x)$p.value
  y <- cumsum(x)  # has unit root
  pval[i,3]=adf.test(y)$p.value
  pval[i,4]=pp.test(y)$p.value
}

colnames(pval)=c("adf-x","pp-x","adf-y","pp-y")

# Draw histogram of p-values
par(mfrow=c(2,2))
for (j in 1:4){hist(pval[,j])}
par(mfrow=c(1,1))

# Rejection frequencies
apply(pval<0.05,2,mean)

## Here, First two columns are powers of the unit root tests
## and the last two columns are sizes of the unit root tests.

### For this particular type of data, pp test is "better" than ADF test.
### But in other cases, ADF test may be better than pp test.
### Keep in mind that statistical tests are NOT perfect in many situations!
### Therefore, try to avoid making a decision solely based on one statistical test.
### Look at plots and other tests. Repeat the experiment if possible.