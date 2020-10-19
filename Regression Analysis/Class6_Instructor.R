
######################################################################################
########Example 1: Toluca company data################################################
######################################################################################


toluca = read.table("/Users/bjkim/MTU_Stat/Teaching/Regression_Analysis_Fall2020/Materials/Class 2/Chapter1_Data_Sets/Toluca_Company.csv",sep=",",header=TRUE)

View(toluca)

### five summary statistics for lot size and work hours ###
summary(toluca$Lot_Size)
summary(toluca$Work_Hour)


### Scatter plot between Lot Size (predictor) and Work Hours (response) ###

plot(toluca$Lot_Size, toluca$Work_Hour, xlim=c(0, 150), ylim=c(0,600),
  	xlab="Lot Size", ylab="Work Hours", pch=19, col="blue")

### Fit a simple linaer regression line ###

Reg_toluca <- lm(Work_Hour~ Lot_Size, data=toluca)

### Add the fitted line to the scatter plot ###

abline(Reg_toluca,lty=1,col="black")


################### Class 6 ###################################


### Show the summary table of the fitted model ### 

summary(Reg_toluca)


##############################################################
##############################################################

##### Regression coefficients in the model #####################

Reg_toluca$coefficients

##### fitted values given at each level of X in samples   ####

Reg_toluca$fitted.values


########### residuals from the estimated regression model ####

Reg_toluca$residuals

summary(Reg_toluca$residuals)

plot(Reg_toluca$residuals, ylab="Residuals", xlab="Index", main="Scatter plot of \n residuals vs. index",cex=1.5,pch=18)

abline(h=0,lty=2,lwd=2)

##############################################################
##############################################################

################ No-intercept model ##########################

Reg0_toluca <- lm(Work_Hour~ Lot_Size +0 , data=toluca)

print(Reg0_toluca)   ### Any change of the estimated slope? ####

summary(Reg0_toluca)

### Scatter plot between Lot Size (predictor) and Work Hours (response) ###

plot(toluca$Lot_Size, toluca$Work_Hour, xlim=c(0, 150), ylim=c(0,600),
     xlab="Lot Size", ylab="Work Hours", pch=19, col="blue")

abline(Reg0_toluca,lty=1,col="red")
abline(Reg_toluca,lty=2,col="orange",lwd=2)

legend("topleft",c("intercept","no intercept"),lty=c(2,1),col=c("red","orange"),
       lwd=c(2,1))


######################################################################################
########Example 2: Crime rate data####################################################
######################################################################################

crimerate = read.table("/Users/bjkim/MTU_Stat/Teaching/Regression_Analysis_Fall2020/Materials/Class 2/Chapter1_Data_Sets/CH01PR28.csv",sep=",",header=TRUE)

colnames(crimerate) <- c("crimerate","percentage")

summary(crimerate$percentage)
summary(crimerate$crimerate)

plot(crimerate$percentage, crimerate$crimerate, 
  	xlab="High School Percentage", ylab="Crime Rate", pch=19, col="blue")

Reg_crimerate <- lm(crimerate ~ percentage, data=crimerate)

summary(Reg_crimerate)


plot(crimerate$percentage, crimerate$crimerate, 
  	xlab="High School Percentage", ylab="Crime Rate", pch=19, col="blue")

abline(lm(crimerate$crimerate ~ crimerate$percentage), col="red")

 

######################################################################################
########Example 3: Mother-daughter heights ###########################################
######################################################################################
######################################################################################
######################################################################################

Heights = read.table("/Users/bjkim/MTU_Stat/Teaching/Regression_Analysis_Fall2020/Materials/Class 2/Chapter1_Data_Sets/Heights.csv",sep=",",header=TRUE)


plot(dheight ~ mheight, data=Heights, col="blue")

abline(lm(Heights$dheight ~ Heights$mheight), col="red")


Reg_Heights <- lm(dheight ~ mheight, data=Heights)

summary(Reg_Heights)



