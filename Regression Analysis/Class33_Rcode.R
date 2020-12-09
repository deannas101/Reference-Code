####################### Surgical Unit Example ########################

############### Read the dataset into R ############################

SU <- read.table("CH09TA01.txt", sep="", header = FALSE)
colnames(SU) <- c(paste0("X",1:8),"Y","logY")


####################### Fit a regression model with all of the predictors ###################

full.lmfit <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8, data=SU)
summary(full.lmfit)

#################### Model Selection (Stepwise Regression) #####################

#### Install packages for the model selection ####

install.packages("leaps")
install.packages("HH")
install.packages("StepReg")

#### Load HH, leaps, and StepReg packages ####

library(leaps)
library(HH)
library(StepReg)

par(mfrow=c(1,1))
#### Stepwise Regression ####

#### Adjusted R2 ####

b = bestsubset(data=SU,y="Y",exclude="logY",select="adjRsq",best=5)
print(b)
stepwise(data=SU,y="Y",exclude="logY",select="adjRsq")
plot(b[,1:2])

#### Cp ####

b = bestsubset(data=SU,y="Y",exclude="logY",select="CP",best=5)
print(b)
stepwise(data=SU,y="Y",exclude="logY",select="CP")
plot(b[,1:2])

#### AIC ####

b = bestsubset(data=SU,y="Y",exclude="logY",select="AIC",best=5)
print(b)
stepwise(data=SU,y="Y",exclude="logY",select="AIC")
plot(b[,1:2])

#### BIC ####

b = bestsubset(data=SU,y="Y",exclude="logY",select="BIC",best=5)
print(b)
stepwise(data=SU,y="Y",exclude="logY",select="BIC")
plot(b[,1:2])


#### Final Model? ####

reduced.lmfit <- lm(Y ~ X1 + X2 + X3 + X8, data=SU)
summary(reduced.lmfit)

######## Regression Diagnostics ############

res <- rstudent(reduced.lmfit)
fitted.y <- fitted(reduced.lmfit)

######## Residual Plots ##########

par(mfrow=c(2,2))
plot(res ~ SU$X1, xlab="X1", ylab="Residual", main="Residuals vs. X1")
abline(h=0)
plot(res ~ SU$X2, xlab="X2", ylab="Residual", main="Residuals vs. X2")
abline(h=0)
plot(res ~ SU$X3, xlab="X3", ylab="Residual", main="Residuals vs. X3")
abline(h=0)
plot(res ~ SU$X8, xlab="X8", ylab="Residual", main="Residuals vs. X8")
abline(h=0)

plot(res ~ fitted.y, xlab="Fitted value", ylab="Residual", main="Residuals vs. Fitted Values")
abline(h=0)

######### Multicollinearity ##########

vif(reduced.lmfit) ### OK ###


######### Constancy of Error Variances #########

bptest(reduced.lmfit)

######### Normality ###########

qqnorm(res);qqline(res)
shapiro.test(res)


######### Transformation #########
library(EnvStats)

boxcox.summary <- boxcox(reduced.lmfit, optimize=TRUE)
lambda <- boxcox.summary$lambda

trans.Y <- SU$Y^lambda

SU <- cbind(SU,trans.Y)

######### Re-fitting a model using the transformed response variable. ##########

boxcox.lmfit <- lm(trans.Y ~ X1 + X2 + X3 + X8, data=SU)
summary(boxcox.lmfit)

boxcox.res <- rstudent(boxcox.lmfit)

boxcox.fitted.y <- fitted(boxcox.lmfit)

######## Residual Plots ##########

par(mfrow=c(2,2))
plot(boxcox.res ~ SU$X1, xlab="X1", ylab="Residual", main="Residuals vs. X1")
abline(h=0)
plot(boxcox.res ~ SU$X2, xlab="X2", ylab="Residual", main="Residuals vs. X2")
abline(h=0)
plot(boxcox.res ~ SU$X3, xlab="X3", ylab="Residual", main="Residuals vs. X3")
abline(h=0)
plot(boxcox.res ~ SU$X8, xlab="X8", ylab="Residual", main="Residuals vs. X8")
abline(h=0)

plot(boxcox.res ~ boxcox.fitted.y, xlab="Fitted value", ylab="Residual", main="Residuals vs. Fitted Values")
abline(h=0)

######### Multicollinearity ##########

vif(boxcox.lmfit) ### OK ###


######### Constancy of Error Variances #########

bptest(boxcox.lmfit)

######### Normality ###########

qqnorm(boxcox.res);qqline(boxcox.res)
shapiro.test(boxcox.res)

######### Final Model ##########

final.lmfit <- boxcox.lmfit
summary(final.lmfit)


