
############################ Class 25 ################################

##################### Bank Example ###################################
######################################################################
#################### Read a data into R. #############################

Bank <- read.table("CH03TA04.txt",sep="",header=FALSE)
colnames(Bank) <- c("X","Y")

######################### Full Model #################################

factor(Bank$X)
full.lmfit <- lm(Y~factor(X), data=Bank)
anova(full.lmfit)

######################### Reduced Model ##############################

reduced.lmfit <- lm(Y~X, data=Bank)
reduced.lmfit
anova(reduced.lmfit)

####################### Lack-of-Fit Test ######################

anova(reduced.lmfit,full.lmfit)


##################### Blaisdell Company Example #######################
#######################################################################
#################### Read a data into R. ##############################

BC <- read.table("CH12TA02.txt",sep="",header=FALSE)

BC <- cbind(seq(1:nrow(BC)),BC)
colnames(BC) <- c("t","Y", "X")

#################### Fit a linear regression model. ###################

lmfit <- lm(Y~X,data=BC)
plot(BC$X, BC$Y, ylab="Company Sales", xlab="Industry Sales", 
     main="Scatter plot of Company Sales against Industry Sales")
abline(lmfit)

#################### Obtain residuals from the estimated function. ##############

BC.res <- resid(lmfit)
plot(BC$t, BC.res, xlab="Time", ylab="Residual", main="Residual plot against time")
abline(h=0)

#################### Obtain residuals from the estimated function. ##############

################# Runs Test #########################
install.packages("lawstat")
library(lawstat)

################# Two-sided #########################
runs.test(BC.res)

################# One-sided #########################
runs.test(BC.res, alternative = "positive.correlated")

runs.test(BC.res, alternative = "negative.correlated")

################################# DW Test ######################################

install.packages("lmtest")
library(lmtest)

################# One-sided Test for Positive Autocorrelation ##################

dwtest(lmfit)

################# One-sided Test for Negative Autocorrelation ##################

dwtest(lmfit, alternative=c("less"))

################# Two-sided Test for Autocorrelation ##################

dwtest(lmfit, alternative=c("two.sided"))
