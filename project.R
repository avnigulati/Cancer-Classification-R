#Project for Summer Introduction to Data Science course SC101
#Script to find the accuracy and Kappa on Wisconsin Cancer Data Set using different methods
#This is a classification problem. 
#There are 30 features that predict whether the Diagnosis is M (malign) or B (Benign)
#Project Members: Neha Dewan, Avni Gulati, Anuradha Dharapuram

#Methodology
#Load caret library
#Read the csv
#Check if the data set has any nulls
#Create models using "kknn","nnet","adaboost","xgbLinear","svmLinearWeights", "mlpWeighDecay","AdaBag"
#Draw boxplot for accuracy and Kappa

library(caret)
system.time(myData=read.csv(file="data.csv",head=TRUE))
View(myData)

#Removing Null Column
myData$X <- NULL
str(myData)

if (myData[complete.cases(myData), ] == nrow(myData)) then {NAStatus="No NA"} else {NAStatus="NA Present"}

#print (myData$diagnosis)
myData$diagnosis = as.character(myData$diagnosis)
#Avni, Neha - For testing the for loop and output creation, I have selected two methods. 
#When everything is good to go, all the 7 methods will be in Methods Vector
#Methods =c("kknn","nnet","adaboost","xgbLinear","svmLinearWeights", "mlpWeighDecay","AdaBag")
Methods =c("kknn","nnet","svmLinearWeights")
MethodCtr=length(Methods)

str(myData)
train_control <- trainControl(method="repeatedcv", number= 10, repeats = 5)
# Build each model   
for (i in 1 : MethodCtr)
{
  timing=system.time(m1 <- train(diagnosis~., data=myData, trControl=train_control, method=Methods[i]))
  message ("-------------------", "Method: ", Methods[i], ": ","user: ",timing[1], " system: ", timing[2], " elapsed: ", timing[3], "--------------------")
}
  
# system.time(m1 <- train(diagnosis~., data=myData, trControl=train_control, method="kknn"))
# system.time(m2 <- train(diagnosis~., data=myData, trControl=train_control, method="nnet"))
# system.time(m3 <- train(diagnosis~., data=myData, trControl=train_control, method="adaboost"))
# system.time(m4 <- train(diagnosis~., data=myData, trControl=train_control, method="xgbLinear"))
# system.time(m5 <- train(diagnosis~., data=myData, trControl=train_control, method="svmLinearWeights"))
# system.time(m6 <- train(diagnosis~., data=myData, trControl=train_control, method="mlpWeightDecay"))
# system.time(m7 <- train(diagnosis~., data=myData, trControl=train_control, method="AdaBag"))

#All Models boxplot
allModels=resamples(list(KNearestNeighbor=m1,NeuralNetwork=m2, Adaboost= m3, XGBoost= m4, SVM= m5, MultiPerceptron= m6, BaggedAdaboost= m7))
bwplot(allModels,scales=list(relation="free"))
