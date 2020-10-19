##### 5.3 Computing ############

# Use the 'c' function to combine numbers into a vector
observed <- c(0.22, 0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
              0.62, 0.99, -0.18, 0.32, 0.34, -0.30, 0.04, -0.87,
              0.55, -1.30, -1.15, 0.20)
predicted <- c(0.24, 0.78, -0.66, 0.53, 0.70, -0.75, -0.41, -0.43,
               0.49, 0.79, -1.19, 0.06, 0.75, -0.07, 0.43, -0.42,
               -0.25, -0.64, -1.26, -0.07)
residualValues <- observed - predicted
summary(residualValues)

# Plot observed values versus predicted values
# It is a good idea to plot the values on a common scale.
axisRange <- extendrange(c(observed, predicted))
plot(observed, predicted,ylim = axisRange,xlim = axisRange)

# Add a 45 degree reference line
abline(0, 1, col = "darkgrey", lty = 2)  ##intercept, slope

# Predicted values versus residuals
plot(predicted, residualValues, ylab = "residual")
abline(h = 0, col = "darkgrey", lty = 2)

## Extends a numerical range by a small percentage, 
## i.e., fraction, on both sides.
x <- 1:5
r <- range(x)           # 1    5
extendrange(x)          # 0.8  5.2
extendrange(x, f= 0.01) # 0.96 5.04

##############

library(caret)
R2(predicted, observed)    ##need package "caret"
RMSE(predicted, observed)  ##need package "caret"

# Sample correlation
corr=cor(predicted, observed)
corr
corr^2

# Rank correlation
cor(predicted, observed, method = "spearman")

######## correlation and Spearman’s rank correlation ########

X=c(106, 86, 100, 101, 99, 103, 97, 113, 112, 110)
Y=c(7, 0, 27, 50, 28, 29, 20, 12, 6, 17)
x=rank(X)
y=rank(Y)
r=cor(X, Y)
rho=cor(x,y)
plot(X,Y)


