######## chapter 4 ###########

### Lesson 2.1
## The two-class data shown in Fig. 4.1 can be obtained using
library(AppliedPredictiveModeling)
data(twoClassData)

## The predictors stored in a data frame called predictors. 
## There are two columns for the predictors and 208 samples in
## rows. The outcome classes are contained in a factor vector called classes.
str(predictors)
str(classes)
nrow(predictors)

## The base R function sample can create simple random splits of the data.
## To create stratified random splits of the data (based on the classes), the
## createDataPartition function in the caret package can be used. The percent
## of data that will be allocated to the training set should be specified.

# Set the random number seed so we can reproduce the results
set.seed(1)
library(caret)

# By default, the numbers are returned as a list. Using
# list = FALSE, a matrix of row numbers is generated.
# These samples are allocated to the training set.

trainingRows <- createDataPartition(classes, p = .80, list= FALSE)
head(trainingRows)
nrow(trainingRows)

# Subset the data into objects for training using
# integer sub-setting

trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]

# Do the same for the test set using negative integers.
testPredictors <- predictors[-trainingRows, ]
testClasses <- classes[-trainingRows]
str(trainPredictors)
str(testPredictors)

########### Resampling #########

## The caret package has various functions for data splitting. For example, to
## use repeated training/test splits, the function createDataPartition could be
## used again with an additional argument named times to generate multiple
## splits.

set.seed(1)
# For illustration, generate the information needed for three
# resampled versions of the training set.

repeatedSplits <- createDataPartition(trainClasses, p = .80, times = 3)
str(repeatedSplits)
repeatedSplits$Resample1

## The caret package has functions createResamples (for bootstrapping),
## createFolds (for k-fold cross-validation) and createMultiFolds (for repeated
## cross-validation). To create indicators for 10-fold cross-validation.

set.seed(1)
cvSplits <- createFolds(trainClasses, k = 10, returnTrain = TRUE)
str(cvSplits)
cvSplits

# Get the first set of row numbers from the list.
fold1 <- cvSplits[[1]]
### or 
fold11=cvSplits$Fold01
length(fold1)

## To get the first 90% of the data (the fold):
cvPredictors1 <- trainPredictors[fold1,]
cvClasses1 <- trainClasses[fold1]
nrow(trainPredictors)
nrow(cvPredictors1)

########## Basic Model Building in R ##########

## Fit a 5-nearest neighbor classification model (Fig. 4.3) to the training data 
## and use it to predict the test set. 
## There are multiple R functions for building this model: 
## the knn function in the MASS package, 
## the ipredknn function in the ipred package, and 
## the knn3 function in caret. 
## The knn3 function can produce class predictions as well asc
## the proportion of neighbors for each class.


## For knn3, we can estimate the 5-nearest neighbor model with

trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x = trainPredictors, y = trainClasses, k = 5)
knnFit

## At this point, the knn3 object is ready to predict new samples. To assign
## new samples to classes, the predict method is used with the model object.

testPredictions <- predict(knnFit, newdata = testPredictors, type = "class")
head(testPredictions)

str(testPredictions)

## Prediction accuracy

sum(testPredictions==testClasses)/length(testClasses)

################ Determination of Tuning Parameters ############

## R has several functions and packages for tuning parameters
## The e1071 package contains the tune function, 
## the errorest function in the ipred package can resample single models. 
## The train function in the caret package has built-in modules
## for 144 models and includes capabilities for different resampling methods,
## performances measures, and algorithms for choosing the best model from the
## profile. 

## Section 4.6 illustrated parameter tuning for a support vector machine using
## the credit scoring data. Using resampling, a value of the cost parameter was
## estimated.
## the SVM model is characterized by what type of kernel function the model uses

## For the credit scoring data, a radial basis function (RBF) kernel
## function was used. This kernel function has an additional tuning parameter
## associated with it denoted as s,
## However, Caputo et al. (2002) describe an
## analytical formula that can be used to get reasonable estimates of s. The
## caret function train uses this approach to estimate the kernel parameter,
## leaving only the cost parameter for tuning.

## Both the training set predictors and outcome are
## contained in an R data frame called GermanCreditTrain.
library(caret)
data(GermanCredit)
str(GermanCredit)

#################

trainingRows <- createDataPartition(GermanCredit$Class, p = .80, list= FALSE)
head(trainingRows)
nrow(trainingRows)

# Subset the data into objects for training using
# integer sub-setting.

GermanCreditTrain <- GermanCredit[trainingRows, ]
str(GermanCreditTrain)

trainClasses <- GermanCreditTrain$Class


######
ClassColNum <- grep("Class", names(GermanCreditTrain))
ClassColNum
trainPredictors=GermanCreditTrain[,-ClassColNum]
str(trainPredictors)

########
# Do the same for the test set using negative integers.
GermanCreditTest <-GermanCredit[-trainingRows, ]

testClasses <- GermanCreditTest$Class
testPredictor=GermanCreditTest[,-ClassColNum]
str(testPredictor)
str(testClasses)

#################
### data(GermanCreditTrain)

set.seed(1056)

## error to run this, GermanCreditTrain is not avalible.
## changed all GermanCreditTrain to GermanCredit.

#######
install.packages("kernlab")
library(kernlab)

svmFit <- train(Class ~ ., data = GermanCreditTrain,
                # The "method" argument indicates the model type.
                # See ?train for a list of available models.
                method = "svmRadial")


## we would like to pre-process the predictor data by
## centering and scaling their values. To do this, the preProc argument can be used:
set.seed(1056)
svmFit <- train(Class ~ ., data = GermanCreditTrain,
                method = "svmRadial", preProc = c("center", "scale"))

## Using the option tuneLength = 10, the cost values 2^-2, 2^-1, . . . 2^7 are evaluated.
set.seed(1056)
svmFit <- train(Class ~ ., data = GermanCreditTrain, method = "svmRadial",
                preProc = c("center", "scale"), tuneLength = 10)

## By default, the basic bootstrap will be used to calculate performance measures.
## Repeated 10-fold cross-validation can be specified with the trainControl
## function. The final syntax is then
set.seed(1056)
svmFit <- train(Class ~ ., data = GermanCreditTrain, method = "svmRadial",
                preProc = c("center", "scale"), tuneLength = 10,
                trControl = trainControl(method = "repeatedcv", repeats = 5))
svmFit

## Figure 4.12 shows an example visualization created from the syntax
# A line plot of the average performance

plot(svmFit, scales = list(x = list(log = 2)))

## To predict new samples with this model, the predict method is called

##########

######
predictedClasses <- predict(svmFit, GermanCreditTest)

sum(predictedClasses==GermanCreditTest$Class)/length(predictedClasses)
sum(predictedClasses=="Good")/length(predictedClasses)

sum(GermanCreditTest$Class=="Good")/length(predictedClasses)

##################
## Use the "type" option to get class probabilities
## the following program is not working
## predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
## head(predictedProbs)

########### Between-Model Comparisons ##############

## While basic logistic regression has no tuning parameters, resampling can still
## be used to characterize the performance of the model. The train function is
## once again used, with a different method argument of "glm" (for generalized
## linear models). The same resampling specification is used and, since the random
## number seed is set prior to modeling, the resamples are exactly the same
## as those in the SVM model.

set.seed(1056)
logisticReg <- train(Class ~ ., data = GermanCreditTrain,
                     method = "glm", 
                     trControl = trainControl(method = "repeatedcv", repeats = 5))

## To compare these two models based on their cross-validation statistics,
## the resamples function can be used with models that share a common set of
## resampled data sets

## First, we create a resamples object from the models:
resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg))
summary(resamp)

## To assess possible differences between the models, the diff method is used:
modelDifferences <- diff(resamp)
summary(modelDifferences)













