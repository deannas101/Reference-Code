
############### Read a data into R ############################

Ridership <- read.table("CH03TA01.txt",sep="", header = FALSE)
colnames(Ridership) <- c("Y","X")

############### Fit a simple linear regression model. #############

lmfit <- lm(Y~X,data=Ridership)

############### Obtain the fitted values of Y. ####################

fit.Y <- fitted(lmfit)

############### Obtain Jackniffed residuals from the model. ############

res <- rstudent(lmfit)


############### Draw Scatter plots. #####################################

par(mfrow=c(1,3))

plot(Ridership$Y ~ Ridership$X, xlab="X",ylab="Y", main="Scatter Plot of Y vs. X")
abline(lmfit)
plot(res ~ fit.Y, xlab="Fitted values", ylab="Residual", main="Residual Plot against Fitted values")
abline(h=0, lwd=2)
plot(res ~ Ridership$X, xlab="X", ylab="Residual", main="Residual Plot against X")
abline(h=0, lwd=2)


