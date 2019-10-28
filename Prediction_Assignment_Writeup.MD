---
#Title: "Prediction Assignment Writeup"
#Author: Lin Wei
#date: "10/21/2019"

---
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(e1071)
library(gbm)

#The goal of this project is to predict the manner in which people did the exercise.

#The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

## Pre-processing Data
#Several columns of the raw data set have string contaning nothing, so we delete those columns first, and we also delete the first 7 columns. These features are obviously not related to predict the outcome.

### Read data   
```{r}

set.seed(12345)
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", stringsAsFactors=FALSE, na.strings=c("#DIV/0!"))

training$classe <- as.factor(training$classe)
```


### Data Partition
#process data and split it into training and testing part 70%:30%
#removing near-zero variance predictors

```{r}
training <- training[,-nearZeroVar(training)]
#removing the first sever variables which I thought would be no helpful to the analysis
training <- training[,-c(1,2,3,4,5,6,7)]
#removing predictors with NA values
training <- training[, colSums(is.na(training)) == 0]

inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
newtraining <- training[inTrain,]

newtesting <- training[-inTrain,]

## plot the distribution of outcome variable
qplot(newtraining$classe, col='blue', xlab='classe', ylab='freq', )

```
### prediction
#I will use three model algorithems(random forest decistion tree, Stochastic gradient boosting trees (gbm) and Decision trees with CART (rpart)) to predict the outcome variable and find the best solution. 

```{r}
## model1: class decision tree(rpart)

modrpt <- rpart(classe ~., method = "class", data = newtraining)
predrpt <- predict(modrpt, newtraining[-length(newtraining)], type='class')
confusionMatrix(predrpt, newtraining$classe)

newtesting$classe<- as.factor(newtesting$classe)
predrpt2 <- predict(modrpt, newtesting, type='class')
confusionMatrix(predrpt2, newtesting$classe)

# the overall accuracy to predict the testing data is 0.7368

# model 2: gbm
set.seed(12345)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)
gbmpred <- train(classe ~ ., data=newtraining, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)
gbmpred2 <- predict(gbmpred, newdata=newtesting)
confusionMatrix(gbmpred2, newtesting$classe)

##model3: ramdom forest decison tree
modrf <- train(classe ~., method = "rf", data = newtraining, verbose = TRUE, trControl = trainControl(method="cv"), number = 3)

predrf <- predict(modrf, newtraining[-length(newtraining)])
confusionMatrix(predrf, newtraining$classe)

newtesting$classe<- as.factor(newtesting$classe)
predrf2 <- predict(modrf, newtesting)
confusionMatrix(predrf2, newtesting$classe)

### testing model with testing data
#We will chose rendom forest approach as final model as it has the highest accuracy 99% among the three modeles.
#But we still need to know how this model performs against the test set before expressing a conclusion.
#the expected out-of-sample error is 100-99.66 = 0.34%.

### read testing data 
#Finally, we load the testing data file and predict the reult as the following:

testingcase <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", stringsAsFactors=FALSE, na.strings=c("#DIV/0!"))
testingcase <- testingcase[,-nearZeroVar(testingcase)]
#testingcase$ <- as.factor(training$classe)

#removing the first sever variables which I thought would be no helpful to the analysis
testingcase <- testingcase[,-c(1,2,3,4,5)]
#removing predictors with NA values
testingcase <- testingcase[, colSums(is.na(testingcase)) == 0]

### apply model to the testing data
pred13 <- predict(mod1, testingcase)

pred13


```
