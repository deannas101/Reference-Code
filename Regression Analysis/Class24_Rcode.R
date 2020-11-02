################# Grocery Retailer Data #######################

############### Read a data into R ############################

GR <- read.csv("CH06PR09.csv", header = TRUE)
colnames(GR) <- c("Y","X1","X2","X3")


############## Explanatory Data Analysis ######################

############# 1. histograms of Y and Xs #######################
par(mfrow=c(2,2))

hist(GR$Y,xlab="the total labor hours(Y)", main="Histogram of Y")
hist(GR$X1,xlab="the number of cases shipped(X1)", main="Histogram of X1")
hist(GR$X2,xlab="the indirect costs of the total labor hours as a percentage(X2)", main="Histogram of X2")
hist(GR$X3,xlab="Holiday(X3)", main="Histogram of X3")

################# 2. Boxplots #################################
par(mfrow=c(1,3))

boxplot(GR$Y, xlab="Y", main="Boxplot of Y")
boxplot(GR$X1, xlab="X1", main="Boxplot of X1")
boxplot(GR$X2, xlab="X2", main="Boxplot of X2")

################ 3. Summary Statistics ########################

summary(GR$Y)
summary(GR$X1)
summary(GR$X2)
table(GR$X3)

################ 4. Scatter Plot Matrix #######################

pairs(GR)

################ 5. correaltion matrix ########################

cor(GR)

################ 5. Added-Variable Plots ######################

library(car)
GR.lmfit <- lm(Y ~ X1 + X2 + X3, data=GR)
avPlots(GR.lmfit)

############## Fitting a Linear Regression Model. #########################

GR.lmfit <- lm(Y ~ X1 + X2 + X3, data=GR)

############## 1. Summary of the fitted equation ##########################

summary(GR.lmfit)

############## 2. ANOVA Table #############################################

anova(GR.lmfit)


############## Assumption Checking ########################################

################# Hat matrix and leverages ################################

X <- as.matrix(cbind(1,GR[,-1]))
inv.XX <- solve(t(X)%*%X)
H <- X%*%inv.XX%*%t(X)
lev <- diag(H)

############## Obtain jackkinfed residual #################################

GR_res <- rstudent(GR.lmfit)

par(mfrow=c(1,1))

################# Nonindependence? ########################################
############# Residual plot against time ##################################

plot(GR_res, xlab="time", ylab="residual", main="Plot of residual against time" )
abline(h=0,lwd=2)

################# Nonconstant Variance? ###################################
############# Residual plot against fitted values #########################

GR.fitted <- fitted(GR.lmfit)
plot(GR_res ~ GR.fitted, xlab="fitted values", ylab="residual", 
     main="Plot of residuals against fitted values" )
abline(h=0,lwd=2)


############# Residual plot against predictor variables ###################
par(mfrow=c(1,2))
plot(GR_res ~ GR$X1, xlab="X1", ylab="residual", 
     main="Plot of residuals against X1" )
abline(h=0,lwd=2)

plot(GR_res ~ GR$X2, xlab="X2", ylab="residual", 
     main="Plot of residuals against X2" )
abline(h=0,lwd=2)


############# ABS Residual plot against predictor variables ###################
par(mfrow=c(1,2))
plot(abs(GR_res) ~ GR$X1, xlab="X1", ylab="|residual|", 
     main="ABS residual plot against X1" )

plot(abs(GR_res) ~ GR$X2, xlab="X2", ylab="|residual|", 
     main="ABS residual plot against X2" )

############### Boxplot of residuals ############################

boxplot(GR_res, xlab="residual", main="Boxplot of residuals")


########################### Any Outliers? ################################
##################### high leverage points ###############################

p <- ncol(X)
n <- nrow(GR)

which(lev > 2*p/n)

library(car)
leveragePlots(GR.lmfit)

hist(lev, main="Histogram of leverage points", xlab="leverage")
abline(v=2*p/n, lty=2, lwd=2, col="red")

###################### Detection methods of influential points ############

############## Install a package, called "olsrr" ###########################

install.packages("olsrr")
library(olsrr)

###################### 1. DFFITS ##########################################

ols_plot_dffits(GR.lmfit)

###################### 2. Cook's D ##########################################

ols_plot_cooksd_chart(GR.lmfit)

###################### 3. DFBETAS ##########################################

ols_plot_dfbetas(GR.lmfit)

######################## Non-normality? ###################################
############################### QQ Plot ###################################

qqnorm(GR_res); qqline(GR_res)
hist(GR_res, main="Historgram of residuals", xlab="Residaul",probability = TRUE)
lines(seq(-3,3,length.out = 1000),dnorm(seq(-3,3,length.out = 1000)))



