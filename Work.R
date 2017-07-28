library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(e1071)
library(ranger)

training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))


dim(training)
head(training)
str(training)

#and remove any columns that has NAs throughout

training <- training[, colSums(is.na(training)) == 0]
training <- training[, colSums(is.na(training)) == 0]

traindata <- training[, -c(1:7)]
testdata <- testing[, -c(1:7)]





set.seed(12345)

inTrain <- createDataPartition(traindata$classe, p = 0.7, list = FALSE)
train_d <- traindata[inTrain,]
valid_d <- traindata[-inTrain,]


myControl <- trainControl(method = "cv", number = 5)

model_rpart <- train(classe ~ ., data = train_d, method = "rpart",
                     trControl = myControl)

model_rf <- train(classe ~ ., data = train_d, method = "rf",
                      trControl = myControl)

result_rpart <- predict(model_rpart, valid_d)
result_rf <- predict(model_rf, valid_d)


confusionMatrix(result_rpart, valid_d$classe)$overall['Accuracy']
confusionMatrix(result_rf, valid_d$classe)$overall['Accuracy']


par(mfrow = c(1,2))

plot(model_rpart, main = "model_rpart Accuracy")
plot(model_rf, main = "model_ranger Accuracy")


Predict <- predict(model_rf, testdata)
Predict

match = (predict == testdata$classe)
qplot(testdata[1], testdata[2], data = testdata, color = match)


?fancyRpartPlot
??fancyRpartPlot
fancyRpartPlot(model_rf$finalModel)
summary(model_ranger)






