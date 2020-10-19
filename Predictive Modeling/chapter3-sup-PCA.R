## Resd the data
XX <- read.table(file = "C:\\Users\\qsha\\Desktop\\Teaching\\Predictive-model\\Fall-2018\\data\\PCA.txt")
XX
sigma=var(XX) ## variance-covariance matrix
sigma
QQ=eigen(sigma) ## calculate eigen values and eigen vectors of sigma
lamda=QQ$values 
AA=QQ$vectors 
##first colume of AA is the first eigen vector corresponding to the first eigen valus.
PCs=as.matrix(XX)%*%as.matrix(AA)
PCs
propotion=cumsum(lamda)/sum(lamda)
propotion
## we can find # of PCs needed to explain 99% of total variability
