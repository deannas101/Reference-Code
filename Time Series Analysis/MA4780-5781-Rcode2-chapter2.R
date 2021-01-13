## Chapter 2


#### 1. Generate IID noise
set.seed(12345)
n=100
e <- rnorm(n+1)
plot(e[1:n],type='o',ylab="",xlab="Time",main="A simulated IID noise",ylim=c(-4,4))
abline(h=0,lty=2)

#### 2. Generate a moving avarege process
set.seed(12345)
n=100
e <- rnorm(n+1)
ma <- e[2:(n+1)]+0.8*e[1:n]
plot(ma,type='o',ylab="",xlab="Time",main="A simulated MA process with MA coefficient 0.8",ylim=c(-4,4))
abline(h=0,lty=2)

#### 3. Generate an autoregressive process
set.seed(12345)
n=100
e <- rnorm(n+1)
ar <- rep(0,n)
ar[1]=e[1]
for (i in 2:n){ar[i]=ar[i-1]*0.8+e[i]}
plot(ar,type='o',ylab="",xlab="Time",main="A simulated AR process with AR coefficient 0.8",ylim=c(-2,6))
abline(h=0,lty=2)


#### 4. Generate a random walk
set.seed(12345)
n=100
e <- rnorm(n+1)
rw <- cumsum(e)
plot(rw,type='o',ylab="",xlab="Time",main="A simulated Random Walk")
abline(h=0,lty=2)



#### 5. Generate a model with trend
set.seed(12345)
n=100
e <- rnorm(n+1)
trend=1+0.5*(1:n)+e[1:n]
plot(trend,type='o',ylab="",xlab="Time",main="A simulated process with a linear trend")
abline(h=0,lty=2)

