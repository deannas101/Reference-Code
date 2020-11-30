
######################################################################################
########Example: Blood Pressure Data##################################################
######################################################################################

BP = read.table("CH11TA01.txt",sep="",header = FALSE)
colnames(BP) <- c("Age","Y")

############################ Preliminary Analysis ####################################

plot(BP$Y ~ BP$Age, ylab="Blood Pressure", xlab="Age", main="Scatter Plot")
############################# Fit a linear model #####################################

lmfit <- lm(Y ~ Age, data=BP)
abline(lmfit)

res.lmfit <- resid(lmfit)

plot(res.lmfit ~ BP$Age, ylab="Residual", xlab="Age", main="Residual plot against Age")
abline(h=0)

plot(abs(res.lmfit) ~ BP$Age, ylab="|Residual|", xlab="Age", main="Abs Residual plot against Age")
abline(h=0)

############################# lm(|e| ~ X) #####################################

lmfit.res <- lm(abs(res.lmfit) ~ Age, data=BP)
lmfit.res

############################# Define weights #####################################

W <- diag(1/fitted(lmfit.res)^2) 

########################## Weighted Least Squares ###########################

Y <- matrix(BP$Y, ncol=1)
X <- matrix(cbind(1,BP$Age),ncol=2)

inv.XWX <- solve(t(X)%*%W%*%X)
XWY <- t(X)%*%W%*%Y
b <- inv.XWX%*%XWY

s.b1 <- sqrt(diag(inv.XWX))[2]

############################ Confidence Interval ################################

upper.limit <- b[2] + qt(0.975, 52)*s.b1
lower.limit <- b[2] - qt(0.975, 52)*s.b1


fit.Yw <- sqrt(W)%*%X%*%b
Yw <- sqrt(W)%*%Y
Xw <- sqrt(W)%*%X
w.res <- (Yw-fit.Yw)

plot(Yw ~ Xw[,2], xlab="Xw", ylab="Yw", main="scatter plot")
abline(fit.Yw ~ Xw[,2])

plot(w.res ~ Xw[,2], xlab="Xw", ylab="residual", main="residual plot against X")
abline(h=0)

plot(w.res ~ fit.Yw, xlab="fitted.value", ylab="residual", main="residual plot against fitted values")
abline(h=0)
