## Chapter 1

# Install "TSA" package provided by CC.
install.packages("TSA")
library(TSA)

# Install "astsa" package provided by SS.
install.packages("astsa")
library(astsa)



## Example 1.1 Johnson and Johnson Data in SS.
## Import from the 'astsa' package.
data(jj)
View(jj)
plot(jj, type="o", ylab="Quarterly Earnings per Share",main="Johnson & Johnson")

## Example 1.2 Global Temperature Data in SS.
## Import from the 'astsa' package.
data(gtemp)
View(gtemp)
plot(gtemp, type="o", ylab="Temperature Anomalies (in Celcuis)",main="Global Warming")



# Simulate iid Normal values (Exercise 1.3 on CC p.10)
x=ts(rnorm(n=48))
plot(x,type='o',ylab="iid normal values with mean 0 and variance 1")
abline(h=0,lty=2)





