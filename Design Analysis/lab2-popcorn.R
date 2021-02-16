# codes used in lab 2
# please check lab2.html for the R tutorial 

# run once
#install.packages(c("ggResidpanel","agricolae"))

library("ggResidpanel")
library("agricolae")

# An experiment is conducted on the effect of cooking time on the volume of microwave popcorn.
# Response: The volume of microwave popcorn (centiliters).
# Treatments: Three cooking times (minutes): 1.25, 1.75, 2.25.
# Experimental units: 18 bags of Pop Secret Homestyle microwave popcorn. All bags are labeled 
# as containing 1.5 ounces (42.5 grams ) of popcorn.
# Randomization: 
## 1. Six bags are randomly assigned to each of three cooking times
## 2. The order of popping and determining the volume of popped but unburned popcorn is also randomized.

# load the dataset
## modify the working directory when running the codes on your own device
setwd("~/Desktop/MA4720/week4/lab2") 
popcorn = read.table("popcorn.txt",header=T)
str(popcorn)
# the factor of interest is categorical
popcorn$Time.f = factor(popcorn$Time)

boxplot(Volume~Time.f,data = popcorn)

popcorn.fit = lm(Volume ~ Time.f, data=popcorn)
# check fitting results
summary(popcorn.fit)
# get predicted values
predict(popcorn.fit)
# get residual values
resid(popcorn.fit)

# check model assumptions
resid_panel(popcorn.fit)
plot(popcorn.fit)

# anova, Anova(lmerTest)
popcorn.aov = aov(Volume~Time.f, data = popcorn)
summary(popcorn.aov)

# multiple comparison test
?LSD.test
LSD.test(y = popcorn$Volume, trt = popcorn$Time.f, alpha = 0.01,
         DFerror = 15, MSerror= 188, p.adj = "none",console=TRUE)

?TukeyHSD
TukeyHSD(popcorn.aov, conf.level = 0.99)

?pairwise.t.test
pairwise.t.test(x = popcorn$Volume, g = popcorn$Time.f, 
                p.adj = "bonferroni")
