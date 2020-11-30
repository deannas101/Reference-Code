####################### Insurance Innovation Example ########################

############### Read the dataset into R ############################

INS <- read.table("CH08TA02.txt", sep="", header = FALSE)
colnames(INS) <- c("Y","X1","X2")

####################### Fit a regression model ###################

INS.lmfit <- lm(Y ~ X1 + X2, data=INS )
summary(INS.lmfit)

#################### Scatter plots with the fitted lines #####################

fitted.y <- fitted(INS.lmfit)
mutual.index <- which(INS$X2==0)
stock.index <- which(INS$X2==1)

plot(INS$Y[mutual.index] ~ INS$X1[mutual.index], xlab="Size of Firm",
     ylab="Number of Months Elapsed", ylim=c(0, max(INS$Y)+1), main="Scatter plot")

points(INS$Y[stock.index] ~ INS$X1[stock.index],pch=19)

abline(lm(fitted.y[mutual.index] ~ INS$X1[mutual.index]), lwd=2)
abline(lm(fitted.y[stock.index] ~ INS$X1[stock.index]), lwd=2, col="blue")

legend("bottomleft",c("Stock Firm","Mutual Firm"), pch=c(1,19))

####################### confidence intervals ###########################

confint(INS.lmfit)



############## Fit a regression model with an interaction effect ###############

INS.lmfit.full <- lm(Y ~ X1*X2, data=INS )
summary(INS.lmfit.full)

#################### Scatter plots with the fitted lines #####################

fitted.y <- fitted(INS.lmfit.full)
mutual.index <- which(INS$X2==0)
stock.index <- which(INS$X2==1)

plot(INS$Y[mutual.index] ~ INS$X1[mutual.index], xlab="Size of Firm",
     ylab="Number of Months Elapsed", ylim=c(0, max(INS$Y)+1), main="Scatter plot")

points(INS$Y[stock.index] ~ INS$X1[stock.index],pch=19)

abline(lm(fitted.y[mutual.index] ~ INS$X1[mutual.index]), lwd=2)
abline(lm(fitted.y[stock.index] ~ INS$X1[stock.index]), lwd=2, col="blue")

legend("bottomleft",c("Stock Firm","Mutual Firm"), pch=c(1,19))


####################### Soap Production Lines Example ########################

############### Read the dataset into R ############################

soap <- read.table("CH08TA05.txt", sep="", header = FALSE)
colnames(soap) <- c("Y","X1","X2")

####################### Fit a regression model #####################

soap.lmfit <- lm(Y ~ X1*X2, data=soap)
summary(soap.lmfit)
anova(soap.lmfit)

#################### Scatter plots with the fitted lines #####################

fitted.y <- fitted(soap.lmfit)
P1.index <- which(soap$X2==1)
P2.index <- which(soap$X2==0)

plot(soap$Y[P1.index] ~ soap$X1[P1.index], xlab="Line Speed",
     ylab="Amount of Scrap", ylim=c(0, max(soap$Y)+1), main="Scatter plot")

points(soap$Y[P2.index] ~ soap$X1[P2.index],pch=19)

abline(lm(fitted.y[P1.index] ~ soap$X1[P1.index]), lwd=2)
abline(lm(fitted.y[P2.index] ~ soap$X1[P2.index]), lwd=2, col="blue")

legend("bottomleft",c("Production Line 1","Production Line 2"), pch=c(1,19))

######################## Assumption Checking ###################################

################################ BF test #######################################
############## Comparison of the error variances for each group ################

############################ Obtain residuals. #################################

soap.res <- resid(soap.lmfit)

# 1. Divide the dataset into two groups, according to the type of the production lines #

P1.res <- soap.res[P1.index]
P2.res <- soap.res[P2.index]

########################### Residual plots #####################################

par(mfrow=c(1,2))
plot(P1.res ~ fitted.y[P1.index], xlab="fitted value", ylab="residual",
     main="Residual plot based on Production Line 1")
abline(h=0)
plot(P2.res ~ fitted.y[P2.index], xlab="fitted value", ylab="residual",
     main="Residual plot based on Production Line 2")
abline(h=0)

# 2. Calculate the absolute deviations of the residuals around the median in each group. # 

d1 <- abs(P1.res - median(P1.res))
d2 <- abs(P2.res - median(P2.res))

# 3. Construct a test statistic based on the two-sample t-test. #

t.test(d1,d2)

################### Inferences about Two Regression Lines ####################
################### Similarity between the two regression lines ##############

lmfit.reduced <- lm(Y ~ X1, data=soap)
anova(lmfit.reduced, soap.lmfit)

################## Test for significance of the interaction effect #############

lmfit.reduced2 <- lm(Y ~ X1 + X2, data=soap)
anova(lmfit.reduced2, soap.lmfit)

################# Confidence intervals ######################

confint(lmfit.reduced2)



