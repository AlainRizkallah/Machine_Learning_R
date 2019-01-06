library(pROC)
library("e1071")
library(class)
library(tree)
library(MASS)
library(leaps)
library(caret)

dataset=read.table(file = 'dataR2.csv',header = T, sep=",")
attach(dataset)
YNClassification <- factor(Classification, levels=c(1,2), labels=c("Healthycontrol","Patient" ))
set.seed(42)
#cross validation
train_control = trainControl(method  = "repeatedcv",number  = 5, repeats=5,classProbs = TRUE,savePredictions = T)
train_set = dataset[,c(0:9)]
train_set$YNClassification =YNClassification

#Formulas

full_formula = YNClassification~Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1
opt_formula = YNClassification~Age+BMI+Glucose+Insulin+Leptin+Resistin+MCP.1

#Functions

presentation = function(model){
  print(summary(model))
  print("Informations")
  print(model)
  print(table(model$pred$obs,model$pred$pred))
  ROC=roc(model$pred$obs,model$pred$Patient, thresholds=seq(0.1,1,0.1))
  plot.roc(ROC,print.auc =T,xlab="Specificity",col="red",axes=T)
}


# GLM
log=train(full_formula,data=train_set,method="glm", metric = "Accuracy", trControl=train_control)
presentation(log)

#Optimisation
OptLog = train(opt_formula,data=train_set,method="glm", metric = "Accuracy", trControl=train_control,preProcess=c("pca","center"))
presentation(OptLog)

#KNN
K_Max = nrow(train_set)/2
knn=train(full_formula,data=train_set,method="knn", tuneGrid=expand.grid(k=1:10),metric = "Accuracy", trControl=train_control,preProcess="pca")
plot(knn)
