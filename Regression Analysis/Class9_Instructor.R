

################### Class 9 ###################################

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


### Show the summary table of the fitted model ### 

summary(Reg_toluca)


##############################################################
##############################################################
Reg_toluca$fitted.values

Reg_toluca$coefficients

Reg_toluca$residuals

summary(Reg_toluca$residuals)
##############################################################
##############################################################

########### 95% Confidence Band for Regression Line ##############

####### Sort X & fitted values ########
sort.X <- sort(toluca$Lot_Size)
sort.fitted.Y <-Reg_toluca$fitted.values[order(toluca$Lot_Size)]


########### W-value Based on F-statistic #######################
W2.val <- 2*qf(0.95,2,23)
W.val <- sqrt(W2.val)

########### estimated standard deviation of Fitted values ################### 
Sxx <- sum((sort.X-mean(sort.X))^2)
se.Y.hat <- sqrt((48.82)^2*(1/25 + (sort.X-mean(toluca$Lot_Size))^2/Sxx)) 


########### Compute the confidence band for regression line ###############

upper.band <- sort.fitted.Y + W.val*se.Y.hat
lower.band <- sort.fitted.Y - W.val*se.Y.hat

lines(sort.X,upper.band,col="blue",lty=2,lwd=2); lines(sort.X,lower.band,col="blue",lty=2,lwd=2)


########### 95% Prediction Band for Regression Line ##############

####### Make new observations for X and compute the fitted values ########
new.X <- seq(0,150,length.out=100)
new.Y <- Reg_toluca$coefficients[1] + Reg_toluca$coefficients[2]*new.X


########### Scheffe's Sc Based on F-statistic #######################
S2.val <- 2*qf(0.95,2,23)
S.val <- sqrt(S2.val)

########### estimated standard deviation of Fitted values ################### 
Sxx <- sum((sort.X-mean(sort.X))^2)
se.Y.pred.hat <- sqrt((48.82)^2*(1+1/25 + (new.X-mean(toluca$Lot_Size))^2/Sxx)) 


########### Compute the prediction band for regression line ###############

upper.pred.band <- new.Y + S.val*se.Y.pred.hat
lower.pred.band <- new.Y - S.val*se.Y.pred.hat

lines(new.X,upper.pred.band,col="red",lty=2,lwd=2); lines(new.X,lower.pred.band,col="red",lty=2,lwd=2)



######################################################################################
########Example 2: Copier maintenance data############################################
######################################################################################


copier <- read.table("CH01PR20.csv",sep=",",header=TRUE)
View(copier)
colnames(copier) <- c("minutes","copiers")

cop_lm <- lm(minutes~copiers, data=copier)
anova(cop_lm)

