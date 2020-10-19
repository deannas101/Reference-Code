############## chapter 11 ##############

## will use packeges: AppliedPredictiveModeling, caret, klaR, MASS, pROC, and
## randomForest 

install.packages(c("klaR", "pROC", "randomForest"))

## nnet package can be used to creat neural networks

## For illustration, the simulated data set shown in Fig. 11.1 
## will be used in this section. To create these data, 
## the quadBoundaryFunc function in the AppliedPredictiveModeling
## package is used to generate the predictors and outcomes:

library(AppliedPredictiveModeling)
set.seed(975)
simulatedTrain <- quadBoundaryFunc(500)
simulatedTest <- quadBoundaryFunc(1000)
head(simulatedTrain)
quadBoundaryFunc ## can see the code of the function

## The random forest and quadratic discriminant models will be fit to 
## the data:
library(randomForest)
rfModel <- randomForest(class ~ X1 + X2,
                        data = simulatedTrain,
                        ntree = 2000)
library(MASS) ## for the qda() function, qda: Quadratic Discriminant Analysis
qdaModel <- qda(class ~ X1 + X2, data = simulatedTrain)

## The output of the predict function for qda objects includes 
## both the predicted classes (in a slot called class) and 
## the associated probabilities are in a matrix called posterior.

qdaTrainPred <- predict(qdaModel, simulatedTrain)

names(qdaTrainPred)

head(qdaTrainPred$class)

head(qdaTrainPred$posterior)

qdaTrainPred

sum(qdaTrainPred$class==simulatedTrain$class)/500 ## accuracy rate on training

qdaTestPred <- predict(qdaModel, simulatedTest)
sum(qdaTestPred$class==simulatedTest$class)/1000 ##accuracy on testing set

## add predicted probability to simulatedTrain, name the column as QDAprob

simulatedTrain$QDAprob <- qdaTrainPred$posterior[,"Class1"] 
simulatedTest$QDAprob <- qdaTestPred$posterior[,"Class1"]

## The random forest model requires two calls to the predict function 
## to get the predicted classes and the class probabilities:

rfTestPred <- predict(rfModel, simulatedTest, type = "prob")
head(rfTestPred)

simulatedTest$RFprob <- rfTestPred[,"Class1"]

head(simulatedTest$RFprob)
simulatedTest$RFclass <- predict(rfModel, simulatedTest)
head(simulatedTest$RFclass)

####### Sensitivity and Specificity ###########
## caret has functions for computing sensitivity and specificity. 
## These functions require the user to indicate the role of each
## of the classes:

# Class 1 will be used as the event of interest
library(caret)
sensitivity(data = simulatedTest$RFclass,
           reference = simulatedTest$class,
           positive = "Class1")

specificity(data = simulatedTest$RFclass,
            reference = simulatedTest$class,
            negative = "Class2")

########### Confusion Matrix ###########

## There are several functions in R to create the confusion matrix. 
## The confusionMatrix function in the caret package produces 
## the table and associated statistics:

confusionMatrix(data = simulatedTest$RFclass,
                reference = simulatedTest$class,
                positive = "Class1")

########## Receiver Operating Characteristic Curves #########

## The pROC package (Robin et al. 2011) can create the curve and 
## derive various statistics. First, an R object must be created 
## that contains the relevant information using the pROC function roc. 
## The resulting object is then used to generate the ROC curve 
## or calculate the area under the curve.

library(pROC)
rocCurve <- roc(response = simulatedTest$class,
                predictor = simulatedTest$RFprob,
                ## This function assumes that the second
                ## class is the event of interest, so we
                ## reverse the labels.
                levels = rev(levels(simulatedTest$class)))

## From this object, we can produce statistics (such as the area 
## under the ROC curve and its confidence interval):

auc(rocCurve)
## ci.roc(rocCurve)
## We can also use the plot function to produce the ROC curve itself:

plot(rocCurve, legacy.axes = TRUE)

## By default, the x-axis goes backwards, used
## the option legacy.axes = TRUE to get 1-spec
## on the x-axis moving from 0 to 1

## Also, another curve can be added using
## add = TRUE the next time plot.auc is used.










































