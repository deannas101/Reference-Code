# codes used in lab 3
# please check lab3.html for the R tutorial 

# run once
#install.packages(c("emmeans","gglot2"))

library("ggResidpanel")
library("emmeans")
library("ggplot2")

# modify the working directory when running the codes on your own device
setwd("~/Desktop/MA4720/week6/lab3") 
torque = read.table("torque.txt", header=T)
str(torque)
torque$Method.f = factor(torque$Method) # treatment
torque$Subject.f = factor(torque$Subject) # block

boxplot(Torque ~ Method.f, data = torque)

torque.fit = lm(Torque ~ Method.f + Subject.f,data=torque)
summary(torque.fit)

resid_panel(torque.fit)

ggplot(data=torque, aes(x=Subject.f, y=Torque, group=Method.f)) +
  geom_line(aes(color=Method.f)) + theme_light(base_size = 14)

torque.aov = aov(Torque ~ Method.f + Subject.f,data=torque)
summary(torque.aov)

RE = (3-1)/3*(372+40)/40 # Error DF = 28 > 20
print(RE)

method.emm = emmeans(torque.fit,~Method.f,level = 0.95) # treatment means
method.emm

pwpp(method.emm) + ylab("Method") + theme_light(base_size = 14)

method.emm # obtain the reference for (c1,c2,c3)
# c1*B+c2*N+c3*S
ct.nav = c(-0.5, 1, -0.5) # N-(B+S)/2
contrast(method.emm,list("N-av(B,S)"=ct.nav))

hardness = read.table("hardness-rcbd.txt", header=T)
hardness$metal.f = factor(hardness$metal)
hardness$tip.f   = factor(hardness$tip)

hardness.fit = lm(depression ~ tip.f + metal.f,data=hardness)

resid_panel(hardness.fit)

ggplot(data=hardness, aes(x=tip.f, y=depression, group=metal.f)) +
  geom_line(aes(color=metal.f)) + theme_light(base_size = 14)

hardness.aov = aov(depression ~ tip.f + metal.f,data=hardness)
summary(hardness.aov)

RE = (9+1)/(9+3) * (12 + 3)/(12+1) * (3/4) * (0.385 + 0.080)/0.080 #  Error DF = 9 < 20
print(RE)

metal.emm = emmeans(hardness.fit,~metal.f)
metal.emm

pairs(metal.emm)

pwpp(metal.emm) + ylab("Metal") + theme_light(base_size = 14)

ct12 = c(1,-1,0,0)
ct34 = c(0,0,1,-1)
ctav = c(0.5, 0.5, -0.5, -0.5)
contrast(metal.emm,list("1-2"=ct12,"3-4"=ct34,"av(1,2) - av(3,4)"=ctav))