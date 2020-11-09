
######################################################################################
########Example: Toluca company data##################################################
######################################################################################

toluca = read.table("Toluca_Company.csv",sep=",",header=TRUE)

######################################################################################
##################Example: Plutonium data#############################################
######################################################################################

Pluto <- read.table("CH03TA10.txt",sep="",header=FALSE)
colnames(Pluto) <- c("Y","X")
Pluto <- Pluto[-24,]

################################ BF test ################################################
######################### Obtain a sample mean of X. #################################### 

######## Toluca Company #############

mean.lot <- mean(toluca$Lot_Size)

# best way to split two group is that one has low values and the other has large values of X #

######################### Sort X-values. #################################### 
sort.Lot <- sort(toluca$Lot_Size)
order.Lot <- order(toluca$Lot_Size)

lmfit <- lm(Work_Hour ~ Lot_Size, data=toluca)
lm.res <- resid(lmfit)

sort.res <- lm.res[order.Lot]

# 1. Divide the data set into two groups, according to the level of X #

group1.index <- which((sort.Lot < mean.lot)|(sort.Lot ==mean.lot))  
group1 <- sort.Lot[group1.index]

group2.index <- which((sort.Lot > mean.lot))  
group2 <- sort.Lot[group2.index]

################## residuals for each group ###########################

group1.res <- sort.res[group1.index]
group2.res <- sort.res[group2.index]

# 2. Calculate the absolute deviations of the residuals around the median in each group. # 
  
d1 <- abs(group1.res - median(group1.res))
d2 <- abs(group2.res - median(group2.res))

# 3. Construct a test statistic based on the two-sample t-test. #

t.test(d1,d2)


######## Plutonium Example #############

Pluto.X <- mean(Pluto$X)

# best way to split two group is that one has low values and the other has large values of X #

######################### Sort X-values. #################################### 
sort.X <- sort(Pluto$X)
order.X <- order(Pluto$X)

Pluto.lmfit <- lm(Y ~ X, data=Pluto)
Pluto.lm.res <- resid(Pluto.lmfit)

plot(Pluto.lm.res ~ fitted(Pluto.lmfit), xlab="Fitted", ylab="Residual",pch=19)
abline(h=0)

sort.X <- sort(Pluto$X)
order.X <- order(Pluto$X)
sort.res <- Pluto.lm.res[order.X]

# 1. Divide the data set into two groups, according to the level of X #

group1.index <- which((sort.X < Pluto.X)|(sort.X ==Pluto.X))  
group1 <- sort.X [group1.index]

group2.index <- which((sort.X > Pluto.X))  
group2 <- sort.X [group2.index]

################## residuals for each group ###########################

group1.res <- sort.res[group1.index]
group2.res <- sort.res[group2.index]

# 2. Calculate the absolute deviations of the residuals around the median in each group. # 

d1 <- abs(group1.res - median(group1.res))
d2 <- abs(group2.res - median(group2.res))

# 3. Construct a test statistic based on the two-sample t-test. #

t.test(d1,d2)


############################## BP test #######################################

################ Obtain SSE. #################### 

anova.lmfit = anova(lmfit)
SSE <- anova.lmfit$`Sum Sq`[2]

################ Regress the squared residuals on X ##########################

res.lmfit <- lm(lm.res^2 ~ toluca$Lot_Size)
anova.res2 <- anova(res.lmfit)

SSR.res <- anova.res2$`Sum Sq`[1]

################ Test Statistic ##################

n <- length(toluca$Lot_Size)
chi2 <- (SSR.res/2)/(SSE/n)^2

chi.val <- qchisq(0.95,1)
p.val <- pchisq(chi2,1,lower.tail = FALSE)


library(lmtest) 

################### Toluca Company ##########################

bptest(lmfit)

################### Plutonium Measurement ##########################

bptest(Pluto.lmfit)


################### Toluca Company ##########################
############################# Shapiro-Wilk test ############################

shapiro.test(lm.res)

############################# KS test ############################

#################### Jackknifed Residual #########################
std.res <- rstudent(lmfit)

##### Generate random variable from standard normal distribution. ####

x <- rnorm(length(std.res))

ks.test(std.res,"pnorm",0,1)


plot(ecdf(std.res), ylab="F(e)", xlab="residual", main=" ")
lines(seq(-5,5,length.out = 1000), pnorm(seq(-5,5,length.out = 1000)),type="l",lty=2,lwd=2, col="blue")


################### Plutonium Measurement ##########################
############################# Shapiro-Wilk test ############################

shapiro.test(Pluto.lm.res)

############################# KS test ############################

#################### Jackknifed Residual #########################
std.res <- rstudent(Pluto.lmfit)

##### Generate random variable from standard normal distribution. ####

x <- rnorm(length(std.res))

ks.test(std.res,"pnorm",0,1)


plot(ecdf(std.res), ylab="F(e)", xlab="residual", main=" ")
lines(seq(-5,5,length.out = 1000), pnorm(seq(-5,5,length.out = 1000)),type="l",lty=2,lwd=2, col="blue")

