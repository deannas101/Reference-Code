############## chapter 13 ##############

## The following R packages are discussed in this chapter: caret, earth, kernlab,
## klaR, MASS, mda, nnet, and rrcov.

####### Nonlinear Discriminant Analysis ##########

### QDA is implemented in the qda function in the MASS as well as an outlier-resistant
### version in the QdaCov function in the rrcov package. 
### RDA is available in the rda function in the klaR package, and 
### MDA can be found in the mda package. 
### The syntax for these models is very similar and we will demonstrate their usage 
### by fitting an MDA model to the grant data.

### The tuning parameter is the number of subclasses per class, 
### which do not have to be the same for each class. 
### For example, to fit an MDA model to the grant data with three subpopulations 
### per class:

install.packages(c("glmnet", "pamr", "rms", "sparseLDA", "subselect"))
library(mda)

### simulated data

library(AppliedPredictiveModeling)
library(MASS)
set.seed(975)
quadFunc=function (n, sigma, pp) 
{
    sigma <- matrix(c(1, sigma, sigma, 2), 2, 2)
    tmpData <- data.frame(mvrnorm(n = n, c(1, 0), sigma), 
                          X3=rnorm(n), X4=sample(0:1, size=n, replace=TRUE,
                          prob=c(1-pp,pp)))
    xSeq <- seq(-4, 4, length = 40)
    plotGrid <- expand.grid(x = xSeq, y = xSeq)
    zFoo <- function(x, y) -1 - 2 * x - 0 * y - 0.2 * x^2 + 2 * 
        y^2
    z2p <- function(x) 1/(1 + exp(-x))
    tmpData$prob <- z2p(zFoo(tmpData$X1, tmpData$X2))
    tmpData$class <- factor(ifelse(runif(length(tmpData$prob)) <= 
        tmpData$prob, "Class1", "Class2"))
    tmpData
}
simulatedTrain <- quadFunc(1000, 0.7, 0.1)
simulatedTest <- quadFunc(500, 0.7, 0.1)
head(simulatedTrain)

### Apply mda to the simulated data with 2 subclasses in each category.

mdaModel <- mda(class ~ .,
                ## Reduce the data to the relevant predictors and the
                ## class variable to use the formula shortcut above
                data = simulatedTrain[, -5],
                ## Number of subclasses per class, default is 3. 
                ## Can be a vector with a number for each class.                
                subclasses = 2)
mdaModel

predict(mdaModel,newdata = head(simulatedTrain[, -c(5,6)]))
head(simulatedTrain$class)

## Each of these nonlinear discriminant models can be built and optimal
## tuning parameters can be found using the caret package. The trControl option
## is set as described in Sect. 12.7 and will be used here:

library(caret)
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE) 
## For more than 2 classes, use "summaryFunction = defaultSummary" or 
## delete "summaryFunction = twoClassSummary"

## You can also use, for example
ctrl <- trainControl(method = "repeatedcv",number = 10, repeats = 5,
                     classProbs = TRUE, savePredictions = TRUE) 

## do not need to save predictions)
set.seed(476)
mdaFit <- train(x = simulatedTrain[,1:4], 
                y = simulatedTrain$class,
                method = "mda",
                metric = "ROC",
                tuneGrid = expand.grid(.subclasses = 1:3),
                trControl = ctrl)
mdaFit

 
## For RDA, use method = "rda", tuning parameters are .gamma (Gamma) and .lambda (Lambda)
## For QDA, use method = "qda"

############### Neural Networks #############

## There are many R packages for neural networks, including nnet, RSNNS, qrnn,
## and neuralnet

## The analyses here focus on the nnet package. 
## The syntax is extremely similar to that of the regression models 
## with a few exceptions. 
### 1) The linout argument should be set to FALSE since most classification models use a sigmoidal
### transformation to relate the hidden units to the outputs. 
### 2) The sums of squared errors or entropy estimates model parameters and the logical arguments
### softmax and entropy toggle between the two.

## The package has both a formula interface and an interface for passing
## matrices or data frames for the predictors and the outcome. For the latter,
## the outcome cannot be a factor variable and must be converted to a set of C
## binary indicators. The package contains a function, class.ind, that is useful
## in making this conversion:
library(nnet)
head(simulatedTrain$class)
head(class.ind(simulatedTrain$class))

## Using the formula interface to fit a simple model:
set.seed(800)
nnetMod <- nnet(class ~ X1 + X2 + X3 + X4,
                data = simulatedTrain,
                size = 3, decay = .1)  # size is the number of hidden units
nnetMod
head(simulatedTest)
predict(nnetMod, newdata = head(simulatedTest))

predict(nnetMod, newdata = head(simulatedTest), type = "class")

## When three or more classes are modeled, the basic call to predict produces
## columns for each class.

## train function can be used to tune the model
## over the amount of weight decay and the number of hidden units. 
## model averaging can be used via the stand-alone avNNet function 
## or using train (with method = "avNNet").

nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$.size)
numWts <- (maxSize * (4 + 1) + (maxSize+1)*2) ## 4 is the number of predictors

set.seed(476)
library(caret)
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)

nnetFit <- train(x = simulatedTrain[,1:4], 
                 y = simulatedTrain$class,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale", "spatialSign"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = numWts,
                 trControl = ctrl)
nnetFit
plot(nnetFit)

########## Flexible Discriminant Analysis ############

## The mda package contains a function (fda) for building this model. 
## The model accepts the formula interface and has an option (method) 
## that specifies the exact method for estimating the regression parameters. 
## To use FDA with MARS, there are two approaches. 
## method = mars uses the MARS implementation in the mda package. 
## However, the earth package, previously described
## in Sect. 7.5, fits the MARS model with a wider range of options. 
## Here, load the earth package and then specify method = earth. 
## For example, a simple FDA model for the simulated data 
## could be created as

library(MASS)
set.seed(975)
quadFunc=function (n, sigma, pp) 
{
    sigma <- matrix(c(1, sigma, sigma, 2), 2, 2)
    tmpData <- data.frame(mvrnorm(n = n, c(1, 0), sigma), 
                          X3=rnorm(n), X4=sample(0:1, size=n, replace=TRUE,
                          prob=c(1-pp,pp)))
    xSeq <- seq(-4, 4, length = 40)
    plotGrid <- expand.grid(x = xSeq, y = xSeq)
    zFoo <- function(x, y) -1 - 2 * x - 0 * y - 0.2 * x^2 + 2 * 
        y^2
    z2p <- function(x) 1/(1 + exp(-x))
    tmpData$prob <- z2p(zFoo(tmpData$X1, tmpData$X2))
    tmpData$class <- factor(ifelse(runif(length(tmpData$prob)) <= 
        tmpData$prob, "Class1", "Class2"))
    tmpData
}
simulatedTrain <- quadFunc(1000, 0.7, 0.1)
simulatedTest <- quadFunc(500, 0.7, 0.1)
head(simulatedTrain)

library(mda)
library(earth)
fdaModel <- fda(class ~ X1 + X2 +X3 +X4, data = simulatedTrain,                 
                method = earth)

## Arguments to the earth function, such as nprune, 
## can be specified when calling fda and are passed through to earth. 
## The MARS model is contained in a subobject called fit:

summary(fdaModel$fit)

## Note that the model coefficients shown here have not been post-processed.
## The final model coefficients can be found with coef(fdaModel). 
## To predict:

predict(fdaModel, head(simulatedTest))
head(simulatedTest$class)

## The train function can be used with method = "fda" to tune this model over
## the number of retained terms. Additionally the varImp function from this
## package determines predictor importance in the same manner as for MARS
## models. 

marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)

fdaTuned <- train(x = simulatedTrain[,1:4], 
                   y = simulatedTrain$class,
                   method = "fda",
                   # Explicitly declare the candidate models to test
                   tuneGrid = marsGrid,
                   trControl = trainControl(method = "cv"))

fdaTuned
plot(fdaTuned)
plot(fdaTuned,main="FDA, degree = 1 and nprune = 6")
fdaPred <- predict(fdaTuned, newdata = simulatedTest[,1:4])
confusionMatrix(data = fdaPred,reference =simulatedTest[,6])

############## Support Vector Machines ##########

## There are a number of R packages
## with implementations for support vector machine and other kernel methods,
## including e1071, kernlab, klaR, and svmPath. 
## The most comprehensive of these is the kernlab package.

## The syntax for SVM classification models is largely the same as the 
## regression case. Although the epsilon parameter is only relevant 
## for regression, a few other parameters are useful for classification:
## • Set prob.model=TRUE to get class probabilities.
## • The class.weights argument assigns asymmetric costs to each class. 
##   This can be especially important when one or more specific
##   types of errors are more harmful than others or when there is a 
##   severe class imbalance that biases the model to the majority class. 
##   The syntax here is to use a named vector of weights or costs. 
##   For example, if there was a desire to bias the simulated-data model to
##   detect Class2, then the syntax would be

class.weights = c(Class1 = 1,  Class2= 5)

##   This makes a false-negative error five times more costly than a 
##   false-positive error. Note that the implementation of class weights
##   in ksvm affects the predicted class, but the class probability model 
##   is unaffected by the weights (in this implementation). 

library(MASS)
set.seed(975)
quadFunc=function (n, sigma, pp) 
{
    sigma <- matrix(c(1, sigma, sigma, 2), 2, 2)
    tmpData <- data.frame(mvrnorm(n = n, c(1, 0), sigma), 
                          X3=rnorm(n), X4=sample(0:1, size=n, replace=TRUE,
                          prob=c(1-pp,pp)))
    xSeq <- seq(-4, 4, length = 40)
    plotGrid <- expand.grid(x = xSeq, y = xSeq)
    zFoo <- function(x, y) -1 - 2 * x - 0 * y - 0.2 * x^2 + 2 * 
        y^2
    z2p <- function(x) 1/(1 + exp(-x))
    tmpData$prob <- z2p(zFoo(tmpData$X1, tmpData$X2))
    tmpData$class <- factor(ifelse(runif(length(tmpData$prob)) <= 
        tmpData$prob, "Class1", "Class2"))
    tmpData
}
simulatedTrain <- quadFunc(1000, 0.7, 0.1)
simulatedTest <- quadFunc(500, 0.7, 0.1)
head(simulatedTrain)

## The following code fits a radial basis function to the simulated data set.

set.seed(202)
library(kernlab)
library(caret)
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)
sigmaRangeReduced <- sigest(as.matrix(simulatedTrain[,1:4]))

## sigest estimates the range of values for the sigma parameter 
## which would return good results when used with a Support Vector Machine 
## ksvm). The estimation is based upon the 0.1 and 0.9 quantile 
## of ||x -x'||^2. Basically any value in between those two bounds 
## will produce good results.

## Given a range of values for the "sigma" inverse width parameter 
## in the Gaussian Radial Basis kernel for use with SVM.
## The estimation is based on the data to be used.

svmRGridReduced <- expand.grid(.sigma = sigmaRangeReduced[1],
                                 .C = 2^(seq(-4, 6)))
set.seed(476)
svmRModel <- train(x = simulatedTrain[,1:4], 
                   y = simulatedTrain$class,
                   method = "svmRadial",
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   tuneGrid = svmRGridReduced,
                   fit = FALSE,
                   trControl = ctrl)
svmRModel
plot(svmRModel)

## When the outcome is a factor, the function automatically uses 
## prob.model = TRUE.

## Other kernel functions can be defined via the kernel and kpar arguments.
## Prediction of new samples follows the same pattern as other functions:

library(kernlab)
## newdata = head(simulatedTest)
predict(svmRModel, newdata = simulatedTest[,1:4])
predict(svmRModel, newdata = simulatedTest[,1:4], type = "prob")


############ K-Nearest Neighbors #############

## Fitting a KNN classification model has similar syntax to fitting a regression
## model. In this setting, the caret package with method set to "knn" generates
## the model. The syntax is 
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)
library(caret)
set.seed(476)
knnFit <- train(x = simulatedTrain[,1:4], 
                y = simulatedTrain$class,
                method = "knn",
                metric = "ROC",
                preProc = c("center", "scale"),
                ##tuneGrid = data.frame(.k = c(4*(0:5)+1, 20*(1:5)+1, 50*(2:9)+1)), ## 21 is the best
                tuneGrid = data.frame(.k = 1:50),
                trControl = ctrl)

knnFit
plot(knnFit)

########## Naive Bayes ##########

## The two main functions for fitting the na¨ive Bayes models in R are 
## naiveBayes in the e1071 package and NaiveBayes in the klaR package. 
## Both offer Laplace corrections, but the version in the klaR package 
## has the option of using conditional density estimates that 
## are more flexible.

library(klaR)
nBayesFit <- NaiveBayes(class ~ .,
                        data = simulatedTrain[,-5],
                        ## Should the non-parametric estimate
                        ## be used?
                        usekernel = TRUE,
                        ## Laplace correction value
                        fL = 2)
nBayesFit
predict(nBayesFit, newdata = head(simulatedTest[,-5]))

## By default, the function
## evaluates probabilities with the normal distribution and the nonparametric
## method (and no Laplace correction).
## fL=2: with Laplacian correction

## Naive Bayes in train function

## method = 'nb'
## Type: Classification

## Tuning parameters:

## fL (Laplace Correction)
## usekernel (Distribution Type)
## adjust (Bandwidth Adjustment)
## Required packages: klaR

install.packages("klaR")
library(klaR)
set.seed(476)
nbFit <- train( x = simulatedTrain[,1:4], 
                y = simulatedTrain$class,
                method = "nb",
                metric = "ROC",
                ## preProc = c("center", "scale"),
                ##tuneGrid = data.frame(.k = c(4*(0:5)+1, 20*(1:5)+1, 50*(2:9)+1)), ## 21 is the best
                tuneGrid = data.frame(.fL = 2,.usekernel = TRUE,.adjust = TRUE),
                trControl = ctrl)

nbFit
plot(nbFit)














