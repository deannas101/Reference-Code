
############### Read a data into R ############################

Insurance <- read.table("CH10TA01.txt",sep="", header = FALSE)
colnames(Insurance) <- c("X1","X2","Y")

############### Regress Y and X1 each against X2. #############

lmfit <- lm(Y~X1+X2,data=Insurance)
lmfit_y2 <- lm(Y~X2,data=Insurance)
lmfit_12 <- lm(X1~X2,data=Insurance)

############### Obtain residuals from each model. ############

e_full <- resid(lmfit)
e_y2 <- resid(lmfit_y2)
e_12 <- resid(lmfit_12)

############### Regress e_y2 against e_12. ###################

lmfit_e <- lm(e_y2 ~ e_12)
summary(lmfit_e)


############### Scatter plot of residuals against X1 #########

par(mfrow=c(1,2))
plot(e_full ~ Insurance$X1, xlab="X1", ylab="Residual", main="Residual Plot against X1")

############### Added-variable plot for X1 ###################

plot(e_y2 ~ e_12, xlab="e(X1|X2)", ylab="e(Y|X2)", main="Added-Variable Plot for X1")
abline(lmfit_e)

library(car)
avPlots(lmfit)
