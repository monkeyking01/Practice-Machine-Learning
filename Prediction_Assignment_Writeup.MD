---
#Title: "Prediction Assignment Writeup"
#Author: Lin Wei
#date: "10/21/2019"

---

## R packages I need for this project
  
#The goal of this project is to predict the manner in which people did the exercise.

#The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

## Pre-processing Data
#Several columns of the raw data set have string contaning nothing, so we delete those columns first, and we also delete the first 7 columns: X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window. These features are obviously not related to predict the outcome.


### Read data   
```{r}

set.seed(12345)
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", stringsAsFactors=FALSE, na.strings=c("#DIV/0!"))

training$classe <- as.factor(training$classe)
```


### process data and split it into training and testing part 70%:30%
#removing near-zero variance predictors

```{r}
training <- training[,-nearZeroVar(training)]
#removing the first sever variables which I thought would be no helpful to the analysis
training <- training[,-c(1,2,3,4,5)]
#removing predictors with NA values
training <- training[, colSums(is.na(training)) == 0]

inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
newtraining <- training[inTrain,]

newtesting <- training[-inTrain,]
```
### prediction
#We chose to fit a random forest model. This model provided the most accurate 
#results all along the machine learning course.The cross-validation is set to 
#draw a subset of the data three different times.

```{r}
mod1 <- train(classe ~., method = "rf", data = newtraining, verbose = TRUE, trControl = trainControl(method="cv"), number = 10)

pred1 <- predict(mod1, newtraining[-length(newtraining)])
confusionMatrix(pred1, newtraining$classe)

### testing model with testing data
#We get a very high accuracy of 99% but we still need to know how this model performs against the test set before expressing a conclusion.

pred12 <- predict(mod1, newtesting)
confusionMatrix(pred12, newtesting$classe)


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
