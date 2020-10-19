######################################################################################
########Example 1: Toluca company data################################################
######################################################################################


############## Import the dataset ##############################

toluca = read.table("Toluca_Company.csv",sep=",",header = TRUE)
head(toluca)


############# Simple Linear Regression ##########################

Reg_toluca <- lm(Work_Hour ~ Lot_Size, data=toluca)

############# Design matrix X and a random vector Y #############

n <- nrow(toluca)
Y <- matrix(toluca$Work_Hour,nrow=n,ncol=1)
X <- matrix(c(rep(1,n),toluca$Lot_Size),nrow=n,ncol=2,byrow=FALSE)

head(Y)
head(X)

##############################################################
##############################################################

######### Least Squares Estimator ################

inv.XX <- solve(t(X)%*%X)

b <- inv.XX%*%t(X)%*%Y
b

Reg_toluca$coefficients


######### Fitted values ##########################

Y.hat <- X%*%b 

Y.hat
Reg_toluca$fitted.values

all.equal(as.numeric(Y.hat),as.numeric(Reg_toluca$fitted.values))


######### Residuals ##############################

H <- X%*%inv.XX%*%t(X)
I <- diag(1,nrow=n,ncol=n)

res <- (I-H)%*%Y 
Reg_toluca$residuals

all.equal(as.numeric(res),as.numeric(Reg_toluca$residuals))


######### t-values for the intercept & the slope ###############

summary_toluca <- summary(Reg_toluca)
MSE <- t(Y)%*%(I-H)%*%Y/(n-2)
sb <- sqrt(diag(inv.XX))*rep(sqrt(MSE),2)

t.val <- b/sb

summary_toluca$coefficients
t.val

