
#####################################################################################################################################################
##########Step 1: Read in data#######################################################################################################################
#####################################################################################################################################################

Comp = read.csv("CH06PR18.csv")

colnames(Comp) <- c("Y","X1","X2","X3","X4")

#####################################################################################################################################################
##########Step 2: Descriptive data analysis#######################################################################################################################
#####################################################################################################################################################

summary(Comp)

par(mfrow=c(1,2))
hist(Comp$Y)
plot(density(Comp$Y))

plot(sort(Comp$Y), pch=".", col="blue", cex=7)

pairs(Comp)

par(mfrow=c(2,2))
plot(Y~X1, Comp)
plot(Y~X2, Comp)
plot(Y~X3, Comp)
plot(Y~X4, Comp)

############ Added-Variable Plots #################

library(car)   ### If you don't have this package, please use "install.packages("car"). ###
Reg_Comp <- lm(Y ~ X1+X2+X3+X4, data=Comp)
avPlots(Reg_Comp)

########### correlation matrix ##############

cor(Comp)

#####################################################################################################################################################
##########Step 3: Multiple Linear Regression Analysis########################################################################################################
#####################################################################################################################################################

Reg_Comp <- lm(Y ~ X1+X2+X3+X4, data=Comp)
Reg_Comp

######## residual plots ###########

Comp_resid <- rstudent(Reg_Comp)
plot(Comp_resid, ylab="residuals", xlab="Index", main="Scatter plot of \n residuals vs. index", cex=1, pch=21)
abline(h=0)


######## residual plots against the fitted values ###########

fitted.Y <- fitted.values(Reg_Comp)
plot(Comp_resid ~ fitted.Y, ylab="residuals", xlab="Fitted Values", 
     main="Scatter plot of \n residuals vs. fitted values", cex=1, pch=21)
abline(h=0)

######## residual plots against the predictors ###########

par(mfrow=c(2,2))

for(i in 1:4){
  plot(Comp_resid ~ Comp[,(i+1)], ylab="residuals", xlab=paste0("X",i), 
       main=paste0("Scatter plot of \n residuals vs. X",i), cex=1, pch=21)
  abline(h=0)
}


######## summary #################

Sum_Comp = summary(Reg_Comp)

Sum_Comp


####### ANOVA Table #############

anova(Reg_Comp)

