
#####################################################################################################################################################
##########Step 1: Read in data#######################################################################################################################
#####################################################################################################################################################

Comp = read.csv("CH06PR18.csv")

colnames(Comp) <- c("Y","X1","X2","X3","X4")

################# Matrix Form #####################

Y <- matrix(Comp[,1], ncol=1)
X <- as.matrix(Comp[,-1])
X <- cbind(1,X)

################ LSE ##############################

XtX <- t(X)%*%X
XtY <- t(X)%*%Y
b <- solve(XtX)%*%XtY

Reg_Comp <- lm(Y ~ X1 + X2 + X3 + X4, data=Comp)
Reg_Comp$coefficients

######## t-test for each coefficient ############

sum_Comp <- summary(Reg_Comp)
MSE <- sum((Y-fitted(Reg_Comp))^2)/76  ### from the summary. 
var.b <- MSE*solve(XtX)

s.b <- sqrt(diag(var.b))

####### t.values ##########
t.values <- b/s.b

############## Confidence intervals for each coefficient ##################

lower.CI <- b - qt(0.975, 76)*s.b
upper.CI <- b + qt(0.975, 76)*s.b

confint(Reg_Comp)

############## Confidence limits for the mean response ####################

Xh <- matrix(X[5,], ncol=1)
Xh

Y.hat <- t(Xh)%*%b

s.Y.hat <- sqrt(MSE*(t(Xh)%*%solve(XtX)%*%Xh))

lower.CI.mean <- Y.hat - qt(0.975, 76)*s.Y.hat 
upper.CI.mean <- Y.hat + qt(0.975, 76)*s.Y.hat


############## Prediction limits for the mean response ####################

Y.hat <- t(Xh)%*%b

s.pred <- sqrt(MSE*(1+t(Xh)%*%solve(XtX)%*%Xh))

lower.PI <- Y.hat - qt(0.975, 76)*s.pred 
upper.PI <- Y.hat + qt(0.975, 76)*s.pred

############## Confidence bands for the response surface ####################

Xh.mat <- matrix(t(X[c(5,8,10,12),]), ncol=4)
Y.hat <- t(Xh.mat)%*%b

############## 1. Working-Hotelling CB ####################

n <- nrow(Comp)
p <- nrow(Xh.mat)
s.Y.hat <- sqrt(MSE*diag(t(Xh.mat)%*%solve(XtX)%*%Xh.mat))

W2.val <- p*qf(0.95,p,n-p) 
W.val <- sqrt(W2.val)

lower.CB <- Y.hat - W.val*s.Y.hat
upper.CB <- Y.hat + W.val*s.Y.hat

WH.CB <- as.data.frame(cbind(lower.CB,upper.CB)) 
colnames(WH.CB) <- c("lower.CB","upper.CB")


############## 2. Bonferroni CB ####################

g <- ncol(Xh.mat)
conf.level <- 1-(0.05/(2*g))
B.val <- qt(conf.level,n-p) 

lower.CB <- Y.hat - B.val*s.Y.hat
upper.CB <- Y.hat + B.val*s.Y.hat

Bonf.CB <- as.data.frame(cbind(lower.CB,upper.CB)) 
colnames(Bonf.CB) <- c("lower.CB","upper.CB")

