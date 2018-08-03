library(caret)
myData=read.csv(file="data.csv",head=TRUE)
View(myData)
myData$X <- NULL
str(myData)
#myData[complete.cases(myData), ]
#print (myData$diagnosis)
myData$diagnosis = as.character(myData$diagnosis)

str(myData)
train_control <- trainControl(method="repeatedcv", number= 10, repeats = 5)
m1 <- train(diagnosis~., data=myData, trControl=train_control, method="kknn")
m2 <- train(diagnosis~., data=myData, trControl=train_control, method="nnet")
m3 <- train(diagnosis~., data=myData, trControl=train_control, method="adaboost")
m4 <- train(diagnosis~., data=myData, trControl=train_control, method="xgbLinear")
m5 <- train(diagnosis~., data=myData, trControl=train_control, method="svmLinearWeights")
m6 <- train(diagnosis~., data=myData, trControl=train_control, method="mlpWeightDecay")
m7 <- train(diagnosis~., data=myData, trControl=train_control, method="AdaBag")


allModels=resamples(list(KNearestNeighbor=m1,NeuralNetwork=m2, Adaboost= m3, XGBoost= m4, SVM= m5, MultiPerceptron= m6, BaggedAdaboost= m7))
bwplot(allModels,scales=list(relation="free"))
