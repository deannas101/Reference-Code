####################### Power Cells Example ########################

############### Read the dataset into R ############################

PC <- read.table("CH08TA01.txt", sep="", header = FALSE)
colnames(PC) <- c("Y","X1","X2")

############### Center and scale the predictor variables ###########

x1 <- (PC$X1-mean(PC$X1))/0.4
x2 <- (PC$X2-mean(PC$X2))/10

PC <- cbind(PC,x1^2,x2^2)

####################### correlation ################################

cor(PC$X1,PC$X1^2); cor(PC$X2,PC$X2^2)
cor(PC$x1,PC$`x1^2`); cor(PC$x2,PC$`x2^2`)

##################### scatter plot matrix #########################

pairs(PC[,-c(4:7)])
cor(PC[,-c(4:7)])

####################### Fit a regression model ###################

PC.lmfit <- lm(Y ~ x1 + x2 + `x1^2` + `x2^2` + x1:x2, data=PC )
summary(PC.lmfit)

####################### residual plots ###########################

PC.res <- rstudent(PC.lmfit)
PC.fitted <- fitted(PC.lmfit)

par(mfrow=c(2,2))
plot(PC.res ~ PC.fitted, ylab="residuals", xlab="fitted values", main="Residual against fitted values")
plot(PC.res ~ PC$x1, ylab="residuals", xlab="x1", main="Residual against x1")
plot(PC.res ~ PC$x2, ylab="residuals", xlab="x2", main="Residual against x2")

############# Check the constancy of the error variances ##############

library(lmtest)
bptest(PC.lmfit)

################## Check the adequacy of the model ###################

PC.full <- lm(Y ~ factor(x1):factor(x2),data=PC)
anova.full <- anova(PC.full)
anova.reduced <- anova(PC.lmfit)

SSPE <- anova.full$`Sum Sq`[2]
SSE <- anova.reduced$`Sum Sq`[6]
SSLF <- SSE - SSPE 

n.level <- length(levels(factor(PC$x1)))*length(levels(factor(PC$x2)))
p <- 6
n <- nrow(PC)

F.lack <- (SSLF/(n.level-p))/(SSPE/(n-n.level))  ## F-value 
pf(F.lack,3,2,lower.tail = FALSE)   ## p-value 


####################### Check the normality ###########################

qqnorm(PC.res); qqline(PC.res)
shapiro.test(PC.res)


########### Comparison between the second-order and the first-order models ############

PC.lmfit.first <- lm(Y ~ x1 + x2, data=PC)
summary(PC.lmfit.first)

anova(PC.lmfit.first,PC.lmfit)

########### Transform the first-order model back ###########

PC.lmfit.OG <- lm(Y ~ X1 + X2, data=PC)
summary(PC.lmfit.OG)

confint(PC.lmfit.OG)


################# Body Fat Measurement Data #######################

############### Read the dataset into R ############################

BMI <- read.table("CH07TA01.txt", sep="", header = FALSE)
colnames(BMI) <- c("X1","X2","X3","Y")

X1X2 <- BMI$X1*BMI$X2; X1X3 <- BMI$X1*BMI$X3; X2X3 <- BMI$X2*BMI$X3

BMI <- cbind(BMI,X1X2,X1X3,X2X3)

cor(BMI[,-4])

############### Center the predictor variables #####################

x1 <- BMI$X1 - mean(BMI$X1)
x2 <- BMI$X2 - mean(BMI$X2)
x3 <- BMI$X3 - mean(BMI$X3)

x1x2 <- x1*x2;  x1x3 <- x1*x3; x2x3 <- x2*x3

BMI.c <- cbind(BMI$Y,x1,x2,x3,x1x2,x1x3,x2x3) 
colnames(BMI.c)[1] <- "Y"

BMI.c <- as.data.frame(BMI.c)

cor(BMI.c[-1])

################ Fit a regression model with interaction terms ##################

BMI.lmfit <- lm(Y ~ x1 + x2 + x3 + x1x2 + x1x3 + x2x3, data=BMI)
summary(BMI.lmfit)
anova(BMI.lmfit)

################ Fit a regression model with no interaction terms ###############

BMI.reduced <- lm(Y ~ x1 + x2 + x3, data=BMI)

################ Test for significance of the interaction terms #################

anova(BMI.reduced, BMI.lmfit)
