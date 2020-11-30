
######################################################################################
########Example: Sales Training data##################################################
######################################################################################

sales = read.table("CH03TA07.txt",sep="",header = FALSE)
colnames(sales) <- c("X","Y")

plot(sales$Y ~ sales$X, ylab="Performance Score", xlab="Days of Training", main="Scatter Plot")
############################# Fit a linear model #####################################

lmfit <- lm(Y ~ X, data=sales)

res.lmfit <- resid(lmfit)

plot(res.lmfit ~ sales$X, ylab="Residual", xlab="Days of Training", main="Residual plot against X")
abline(h=0)

############################# Transformation #####################################

root.X <- sqrt(sales$X)
sales <- cbind(sales,root.X)

colnames(sales)[3] <- c("root.X")

sqrt.lmfit <- lm(Y ~ root.X, data=sales)

plot(sales$Y ~ sales$root.X, ylab="Performance Score", xlab=bquote(sqrt(X)), main="Scatter Plot")

abline(sqrt.lmfit)

res.sqrt.lmfit <- resid(sqrt.lmfit)

plot(res.sqrt.lmfit  ~ sales$X, ylab="Residual", xlab=bquote(sqrt(X)), main="Residual plot against X")
abline(h=0)


########################### Assumption Checking #################################

############## Lack of Fit Test (Linearity) ##########################

full.lmfit <- lm(Y ~ factor(root.X), data=sales)
anova(full.lmfit)

reduced.lmfit <- lm(Y ~ root.X, data=sales)
anova(reduced.lmfit)


anova(reduced.lmfit, full.lmfit)

################ Durbin-Watson Test (Independence) ##########################

library(lmtest)
dwtest(sqrt.lmfit)

################ Breusch-Pagan Test (Constant Variance) ##########################

bptest(sqrt.lmfit)


####################### QQ Plot & Shapiro-Wilk Test (Normality) ##################

qqnorm(res.sqrt.lmfit);qqline(res.sqrt.lmfit)
shapiro.test(res.sqrt.lmfit)

####################### Outliers? ########################

std.res <- rstudent(sqrt.lmfit)
plot(std.res ~ fitted(sqrt.lmfit),ylab="residual", xlab="fitted", ylim=c(-2,2),
     main="residual plot against fitted values")
abline(h=0)


######################################################################################
########Example: Plasma Levels data ##################################################
######################################################################################

plasma <- read.table("CH03TA08.txt",sep="",header=FALSE)
colnames(plasma) <- c("X","Y","logY")


plot(plasma$Y ~ plasma$X, ylab="Plasma Level", xlab="Age", main="Scatter Plot")
############################# Fit a linear model #####################################

lmfit <- lm(Y ~ X, data=plasma)

res.lmfit <- resid(lmfit)

plot(res.lmfit ~ plasma$X, ylab="Residual", xlab="Age", main="Residual plot against Age")
abline(h=0)

############################# Transformation #####################################

log.lmfit <- lm(logY ~ X, data=plasma)

plot(plasma$logY ~ plasma$X, ylab=bquote(log(Y)), xlab="X", main="Scatter Plot")
abline(log.lmfit)

res.log.lmfit <- resid(log.lmfit)

plot(res.log.lmfit  ~ plasma$X, ylab="Residual", xlab="X", main="Residual plot against Age")
abline(h=0)


########################### Assumption Checking #################################

############## Lack of Fit Test (Linearity) ##########################

full.lmfit <- lm(logY ~ factor(X), data=plasma)
anova(full.lmfit)

reduced.lmfit <- lm(logY ~ X, data=plasma)
anova(reduced.lmfit)

anova(reduced.lmfit, full.lmfit)


################ Durbin-Watson Test (Independence) ##########################

library(lmtest)
dwtest(log.lmfit, alternative = "two.sided")

################ Breusch-Pagan Test (Constant Variance) ##########################

bptest(log.lmfit)


####################### QQ Plot & Shapiro-Wilk Test (Normality) ##################

qqnorm(res.log.lmfit);qqline(res.log.lmfit)
shapiro.test(res.log.lmfit)

####################### Outliers? ########################

std.res <- rstudent(log.lmfit)
plot(std.res ~ fitted(log.lmfit),ylab="residual", xlab="fitted", ylim=c(-2,2),
     main="residual plot against fitted values")
abline(h=0)

######################################################################################
############ Example: Bacteria data ##################################################
######################################################################################

bacteria <- read.csv("bacteria.csv")

plot(bacteria$survive ~ bacteria$t, ylab="Bacteria", xlab="Time", main="Scatter Plot")
############################# Fit a linear model #####################################

lmfit <- lm(survive ~ t, data=bacteria)

res.lmfit <- resid(lmfit)

plot(res.lmfit ~ bacteria$t, ylab="Residual", xlab="Time", main="Residual plot against Time")
abline(h=0)

qqnorm(res.lmfit);qqline(res.lmfit)
shapiro.test(res.lmfit)

logY <- log(bacteria$survive)

bacteria <- cbind(bacteria,logY)
colnames(bacteria)[3] <- c("logY")

############################# Transformation #####################################

log.lmfit <- lm(logY ~ t, data=bacteria)

plot(bacteria$logY ~ bacteria$t, ylab=bquote(log(Y)), xlab="Time", main="Scatter Plot")
abline(log.lmfit)

res.log.lmfit <- resid(log.lmfit)

plot(res.log.lmfit  ~ bacteria$t, ylab="Residual", xlab="Time", main="Residual plot against Time")
abline(h=0)

qqnorm(res.log.lmfit);qqline(res.log.lmfit)
shapiro.test(res.log.lmfit)

########################### Assumption Checking #################################


################ Durbin-Watson Test (Independence) ##########################

library(lmtest)
dwtest(log.lmfit, alternative = "two.sided")

################ Breusch-Pagan Test (Constant Variance) ##########################

bptest(log.lmfit)


####################### QQ Plot & Shapiro-Wilk Test (Normality) ##################

qqnorm(res.log.lmfit);qqline(res.log.lmfit)
shapiro.test(res.log.lmfit)

####################### Outliers? ########################

std.res <- rstudent(log.lmfit)
plot(std.res ~ fitted(log.lmfit),ylab="residual", xlab="fitted", ylim=c(-2,2),
     main="residual plot against fitted values")
abline(h=0)

######################## Box-Cox Transformations ###############################

install.packages("EnvStats")
library(EnvStats)
boxcox(lmfit, optimize = TRUE)

plot(boxcox.summary$objective ~ boxcox.summary$lambda, xlab=bquote(lambda), ylab="Likelihood",
     main="Likelihood vs. Power (lambda)", type="l",lwd=2)
abline(v=-0.3930,lty=2,col="red")

boxcox.data <- cbind(boxcox.summary$lambda, boxcox.summary$objective)
colnames(boxcox.data) <- c("lambda","Likelihood")

