
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

cor(toluca$Lot_Size,toluca$Work_Hour)

### Fit a simple linear regression line ###

Reg_toluca <- lm(Work_Hour~ Lot_Size, data=toluca)
Reg_toluca


### Add the fitted line to the scatter plot ###

abline(Reg_toluca,lty=2,col="red")

### Show the summary table of the fitted model ### (I will explain it later.)

summary(Reg_toluca)



