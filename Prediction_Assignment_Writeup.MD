---
Title: "Prediction Assignment Writeup"
Author: Lin Wei
date: "10/21/2019"

---

### Read data   
```{r}
library(caret)
library(rpart) 
library(RColorBrewer) 
library(randomForest) 
library(e1071) 
library(gbm)

set.seed(12345)
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", stringsAsFactors=FALSE, na.strings=c("#DIV/0!"))

training$classe <- as.factor(training$classe)
```


### Data Partition

```{r}
training <- training[,-nearZeroVar(training)]
training <- training[,-c(1,2,3,4,5,6,7)]
training <- training[, colSums(is.na(training)) == 0]

inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
newtraining <- training[inTrain,]
newtesting <- training[-inTrain,]
qplot(newtraining$classe, col='blue', xlab='classe', ylab='freq', )

```
### prediction
```{r}
# model1: class decision tree(rpart)
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
gbmfinMod <- gbmpred$finalModel
gbmpred2 <- predict(gbmpred, newdata=newtesting)
confusionMatrix(gbmpred2, newtesting$classe)

#model3: ramdom forest decison tree
modrf <- train(classe ~., method = "rf", data = newtraining, verbose = TRUE, trControl = trainControl(method="cv"), number = 3)

predrf <- predict(modrf, newtraining[-length(newtraining)])
confusionMatrix(predrf, newtraining$classe)

newtesting$classe<- as.factor(newtesting$classe)
predrf2 <- predict(modrf, newtesting)
confusionMatrix(predrf2, newtesting$classe)

### testing model with testing data

testingcase <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", stringsAsFactors=FALSE, na.strings=c("#DIV/0!"))
testingcase <- testingcase[,-nearZeroVar(testingcase)]
testingcase <- testingcase[,-c(1,2,3,4,5)]
testingcase <- testingcase[, colSums(is.na(testingcase)) == 0]

pred13 <- predict(modrf, testingcase)

pred13


```
