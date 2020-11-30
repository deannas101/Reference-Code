################# Body Fat Measurement Data #######################

############### Read the dataset into R ############################

BMI <- read.table("CH07TA01.txt", sep="", header = FALSE)
colnames(BMI) <- c("X1","X2","X3","Y")

################## Explanatory Data Analysis ######################

cor(BMI)
pairs(BMI)

################### Fit a multiple linear regression model ###################

lmfit.full <- lm(Y ~ X1 + X2 + X3, data=BMI)
summary(lmfit.full)
anova(lmfit.full)

#################### Assumption Checking ####################################

res.full <- rstudent(lmfit.full)
fitted.y <- fitted(lmfit.full)

par(mfrow=c(2,2))
plot(res.full ~ fitted.y, xlab="fitted values", ylab="residuals", main="residual plot against fitted values")
plot(res.full ~ BMI$X1, xlab="X1", ylab="residuals", main="residual plot against X1")
plot(res.full ~ BMI$X2, xlab="X2", ylab="residuals", main="residual plot against X2")
plot(res.full ~ BMI$X3, xlab="X3", ylab="residuals", main="residual plot against X3")

################ Constancy of Error Variances #####################

library(lmtest)
bptest(lmfit.full)

################ Normality #####################

shapiro.test(res.full)


################### Diagnose Multicollinearity. ########################

summary(lmfit.full)
cor(BMI)   ###### The signs of some estimates from the output and the correlation matrix are different. 
anova(lmfit.full)

################## Check Variance Inflation Factor. #####################

vif(lmfit.full)

################# Consider other regression models. #####################

lmfit1 <- lm(Y ~ X1 + X2, data=BMI)
lmfit2 <- lm(Y ~ X1 + X3, data=BMI)
lmfit3 <- lm(Y ~ X2 + X3, data=BMI)

summary(lmfit1)
summary(lmfit2)
summary(lmfit3)


################ Comparison between the full model and the reduced models ############

anova(lmfit1, lmfit.full)
anova(lmfit2, lmfit.full)
anova(lmfit3, lmfit.full)

################ VIF on the reduced models ##################

vif(lmfit1) 
vif(lmfit2)
vif(lmfit3)



