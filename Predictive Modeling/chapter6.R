############Section 6.5 R Code ###################

install.packages(c("elasticnet", "lars", "MASS", "pls", "AppliedPredictiveModeling"))
library(AppliedPredictiveModeling)
data(solubility)
??solubility

## The data objects begin with "sol":

ls(pattern = "^solT")
set.seed(2)
sample(names(solTrainX), 8)
dim(solTrainX)
solTrainX[1:5, 1:5]

## Alternate versions of these data that have been Box–Cox transformed
## are contained in the data frames solTrainXtrans and solTestXtrans

####Ordinary Linear Regression####

trainingData <- solTrainXtrans
dim(trainingData)

## Add the solubility outcome

trainingData$Solubility <- solTrainY
dim(trainingData)
trainingData[,229]-solTrainY
lmFitAllPredictors <- lm(Solubility ~ ., data = trainingData)
lmFitAllPredictors
summary(lmFitAllPredictors)

##prediction

lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
head(lmPred1)
lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)

library(caret)

defaultSummary(lmValues1)

## alternative, we can use
sqrt(mean((solTestY-lmPred1)^2))
cor(solTestY, lmPred1)^2

## If we wanted a robust linear regression model, then the robust linear model
## function (rlm) from the MASS package could be used

library(MASS)
rlmFitAllPredictors <- rlm(Solubility ~ ., data = trainingData)
rlmFitAllPredictors 
summary(rlmFitAllPredictors) 

## The train function generates a resampling estimate of performance
## The function trainControl specifies the type of resampling

ctrl <- trainControl(method = "cv", number = 10)  ## 10-fold CV
set.seed(100)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY, method = "lm", trControl = ctrl)
lmFit1
xyplot(solTrainY ~ predict(lmFit1), type = c("p", "g"), xlab = "Predicted", ylab = "Observed")

### plot the points (type = 'p') and a background grid ('g'), use type = c("p", "g")

xyplot(resid(lmFit1) ~ predict(lmFit1), type = c("p", "g"), 
       xlab = "Predicted", ylab = "Residuals")

## Predict on the testing set, find RMSE and R^2
lmPred1 <- predict(lmFit1, solTestXtrans)

lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)

library(caret)

defaultSummary(lmValues1)

### To build a smaller model without predictors with extremely high correlations,
## reduce the number of predictors, such that there are no absolute pairwise correlations above 0.9:

corThresh <- .9
tooHigh <- findCorrelation(cor(solTrainXtrans), corThresh)
corrPred <- names(solTrainXtrans)[tooHigh]
length(tooHigh)
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered <- solTestXtrans[, -tooHigh]

set.seed(100)
##lmFiltered <- train(solTrainXtrans, solTrainY, method = "lm", trControl = ctrl)
lmFiltered <- train(trainXfiltered, solTrainY, method = "lm", trControl = ctrl)
lmFiltered


## Robust linear regression can also be performed using the train function 
## However, rlm does not allow the covariance matrix of the predictors to be singular
## To ensure that predictors are not singular, 
## we will pre-process the predictors using PCA. 

set.seed(100)
rlmPCA <- train(trainXfiltered, solTrainY, method = "rlm", preProcess = "pca", trControl = ctrl)
rlmPCA

#### Partial Least Squares ####

library(pls)
library(AppliedPredictiveModeling)
data(solubility)
trainingData <- solTrainXtrans
trainingData$Solubility <- solTrainY
plsFit <- plsr(Solubility ~ ., data = trainingData)
summary(plsFit)

## The number of components can be fixed using the ncomp argument or, if
## left to the default, the maximum number of components will be calculated

plsFit1 <- plsr(Solubility ~ ., data = trainingData, ncomp=2)
summary(plsFit1)

## Results for varImp are not the same as in the book. Need further investigation ##
library(caret)
plsImp <- varImp(plsFit1, useModel=pls, scale = FALSE)


plot(plsImp , top = 20)
plot(plsImp)

## plsImp <- varImp(plsFit, useModel=pls, scale = FALSE)
## plot(plsImp, top = 25, scales = list(y = list(cex = .95)))
####

x1=order(plsImp, decreasing = TRUE)
y1=plsImp[x1,]
z1=y1[1:40]
plot(z1)
barplot(z1)

## Predictions can be made for a specific number of components or for several
## values at a time.
predict(plsFit, solTestXtrans[1:5,], ncomp = 1:2)

## train can also be used with method values of pls,
set.seed(100)
ctrl <- trainControl(method = "cv", number = 10)
plsTune <- train(solTrainXtrans, solTrainY, method = "pls", tuneLength = 20, 
                 trControl = ctrl, preProc = c("center", "scale"))
plsTune
plot(plsTune)

xyplot(solTrainY ~ predict(plsTune), type = c("p", "g"), xlab = "Predicted", ylab = "Observed") ## predict on training
predicted<-predict(plsTune, solTestXtrans)
lmValues2 <- data.frame(obs = solTestY, pred = predicted)

defaultSummary(lmValues2)

#### Penalized Regression Models ####

## Ridge-regression models can be created using the lm.ridge function in the MASS package 
## or the enet function in the elasticnet package. 
## When calling the enet function, the lambda argument specifies the ridge-regression penalty
library(lars)
library(elasticnet)

library(AppliedPredictiveModeling)
data(solubility)

ridgeModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY, lambda = 0.001)
## the lambda argument specifies the ridge-regression penalty.

## The predict function for enet objects generates predictions for one
## or more values of the lasso penalty simultaneously using the s and mode arguments.
## To produce a ridge-regression solution we define s=1 with mode = "fraction".

ridgePred <- predict(ridgeModel, newx = as.matrix(solTestXtrans), s = 1, 
                     mode = "fraction", type = "fit")

head(ridgePred$fit)

## To tune Ridge regression over the penalty, train can be used with a different method:
## Define the candidate set of values

library(caret)
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 15))
set.seed(100)

ctrl <- trainControl(method = "cv", number = 10)
ridgeRegFit <- train(solTrainXtrans, solTrainY, method = "ridge", 
                     tuneGrid = ridgeGrid, trControl = ctrl,
                     preProc = c("center", "scale"))
## Fit the model over many penalty values
## put the predictors on the same scale

ridgeRegFit

## Lasso model can be estimated using a number of different functions.
## Lars fuction in the lars package; enet in elasticnet package; 

## enetModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY, 
                  lambda = 0.01, normalize = TRUE)

enetModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY, 
                  lambda =0, normalize = TRUE)
## The predictor data must be a matrix object, 
## The predictors should be centered and scaled prior to modeling. 
## The normalize argument will do this standardization automatically
## The parameter lambda controls the ridge-regression penalty and, 
## setting this value to 0, fits the lasso 
## The lasso penalty does not need to be specified until the time of prediction:

enetPred <- predict(enetModel, newx = as.matrix(solTestXtrans), s = .1, 
                    mode = "fraction", type = "fit")

## A list is returned with several items:

names(enetPred)

## The 'fit' component has the predicted values:

head(enetPred$fit)

## To determine which predictors are used in the model, 
## the predict method is used with type = "coefficients":

enetCoef<- predict(enetModel, newx = as.matrix(solTestXtrans), s = .1, 
                   mode = "fraction", type = "coefficients")
                  
tail(enetCoef$coefficients)

## To tune the elastic net model using train, we specify method = "enet". 
## we tune the model over a custom set of penalties:

enetGrid <- expand.grid(.lambda = c(0, 0.01, .1), .fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(solTrainXtrans, solTrainY, method = "enet", 
                  tuneGrid = enetGrid, trControl = ctrl,
                  preProc = c("center", "scale"))
plot(enetTune)
















