######## chapter 3 ###########

install.packages(c("caret", "corrplot", "e1071", "lattice", "AppliedPredictiveModeling"))
library(AppliedPredictiveModeling)

## The raw segmentation data set is contained in the AppliedPredictiveModeling
## package.5 To load the data set into R:

data(segmentationOriginal) 
dim(segmentationOriginal)
segmentationOriginal
segmentationOriginal[1:5,1:20]
names(segmentationOriginal)

## There were fields that identified each cell (called Cell) 
## and a factor vector that indicated which cells were well segmented (Class). 
## The variable Case indicated which cells were originally used for the training and test sets.

## The analysis in this chapter focused on the training set samples, so the data are
## filtered for these cells:
segData <- subset(segmentationOriginal, Case == "Train") 
dim(segData)
segData$Case
segData[1:5, 1:20]


## The Class and Cell fields will be saved into separate vectors, then removed
## from the main object:
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case

# Now remove the columns
segData <- segData[, -(1:3)]
segData[1:5, 1:20]
dim(segData)
names(segData)

## The original data contained several “status” columns which were binary versions
## of the predictors. To remove these, we find the column names containing
## "Status" and remove them:

statusColNum <- grep("Status", names(segData))   ## Pattern Matching and Replacement
statusColNum 
length(statusColNum)
segDataCont <- segData[, -statusColNum]  ## only continous predictors

dim(segDataCont)
names(segDataCont)

#### Transformations ######### 

## To center and scale predictors, scale(x, center = TRUE, scale = TRUE)can be used.
YY = scale(segDataCont)
head(colMeans(YY))
var(YY[,1:5])    ## same as cov

## The skewness function in the e1071 package calculates the sample skewness statistic
## for each predictor:
library(e1071)

# For one predictor:
skewness(segDataCont$AvgIntenCh3)  ## or to use
skewness(segDataCont[,5])

# Since all the predictors are numeric columns, the apply function can
# be used to compute the skewness across columns.
skewValues <- apply(segDataCont, 2, skewness)
head(skewValues)  ##skewValues

## To determine which type of transformation should be used, the MASS
## package contains the "boxcox" function. This function estimates lambda, 
## it does not create the transformed variable(s).
## A caret function, BoxCoxTrans,
## can find the appropriate transformation and apply them to the new data:

library(caret)
AvgIntenCh3Trans <- BoxCoxTrans(segDataCont$AvgIntenCh3) 
AvgIntenCh3Trans 

# The original data
head(segDataCont$AvgIntenCh3)
skewness(segDataCont$AvgIntenCh3)

# After transformation
predict(AvgIntenCh3Trans, head(segDataCont$AvgIntenCh3)) ## The first 6 values after tranformation
x1=predict(AvgIntenCh3Trans, segDataCont$AvgIntenCh3)
skewness(x1)

## manually checking
(69.91688^(.3) - 1)/(.3)

## Another caret function, preProcess, applies this transformation to a set of
## predictors. This function is discussed below.

xx1 <- preProcess(segDataCont, method = c("BoxCox"))
xx1

# Apply the transformations:

transformed <- predict(xx1, segDataCont)

## The caret package class spatialSign contains functionality for the spatial sign
## transformation. The basic syntax would be spatialSign(segData).

## The base R function prcomp can
## be used for PCA. In the code below, the data are centered and scaled prior
## to PCA.

pcaObject <- prcomp(segDataCont, center = TRUE, scale. = TRUE)

# The standard deviations for the columns in the data are stored in pcaObject as a sub-object called ad:
# Calculate the cumulative percentage of variance which each component
# accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:5]

cumpercentVariance <- cumsum(pcaObject$sd^2)/sum(pcaObject$sd^2)*100
cumpercentVariance[1:20]

## The transformed values are stored in pcaObject as a sub-object called x:
head(pcaObject$x[, 1:5])

## The another sub-object called rotation stores the variable loadings, where
## rows correspond to predictor variables and columns are associated with the
## components:
head(pcaObject$rotation[, 1:5])

## The caret class preProcess has the ability to transform, center, scale, or impute values,
## as well as apply the spatial sign transformation and feature extraction. The
## function calculates the required quantities for the transformation. After calling
## the preProcess function, the predict method applies the results to a set
## of data. For example, to perform Box–Cox transform, center, and scale the data, then
## execute PCA, the syntax would be:

trans <- preProcess(segDataCont, method = c("BoxCox", "center", "scale", "pca"))  ## need {caret} package
trans

# Apply the transformations:

transformed <- predict(trans, segDataCont)  # 19 PCs, default value: C = 95%
dim(segDataCont)
dim(transformed)

# These values are different than the previous PCA components since
# they were transformed prior to PCA
head(transformed[, 1:5])

## The order in which the possible transformation are applied is transformation,
## centering, scaling, imputation, feature extraction, and then spatial sign.

##### Filtering #############

## To filter for near-zero variance predictors, the caret package function nearZeroVar
## will return the column numbers of any predictors that fulfill the conditions
## outlined in Sect. 3.5. 
## nearZeroVar(segDatacont) ## This pre-processing should not be performed on continouse predictors

segDataCat<-segData[, statusColNum] ## for all categorical predictors, need to recall the data
dim(segDataCat)
nearZeroVar(segDataCat)
length(which(segDataCat[,33]==0))

# When predictors should be removed, a vector of integers is
# returned that indicates which columns should be removed. 
# In this example, columns 33, 37, and 38 are near zero variance predictors.

## Similarly, to filter on between-predictor correlations, the cor function can
## calculate the correlations between predictor variables:
correlations <- cor(segDataCont)
dim(correlations)
correlations[1:4, 1:4]

## To visually examine the correlation structure of the data, the corrplot package
## contains an excellent function of the same name. 
library(corrplot)
corrplot(correlations, order = "hclust")

## To filter based on correlations, the findCorrelation function will apply the
## algorithm in Sect. 3.5. For a given threshold of pairwise correlations, the function
## returns column numbers denoting the predictors that are recommended
## for deletion:
highCorr <- findCorrelation(correlations, cutoff = .85)
length(highCorr)
highCorr
filteredSegData <- segDataCont[, -highCorr]
length(filteredSegData)

####### Impute missing values #######

## use preProcess
library(caret)
Im <- preProcess(segDataCont,method=c("BoxCox","center","scale","knnImpute")) ## need {caret} package
## Apply inputation
segDataIm <- predict(Im,segDataCont)
## knn can also be used for imputation.
