library(pROC)
library("e1071")
library(class)
library(tree)
library(MASS)
library(leaps)
library(caret)

dataset=read.table(file = 'dataR2.csv',header = T, sep=",")
attach(dataset)

#Functions

getResult = function(model,isDoROC=TRUE){
  res = NULL
  res$summary = summary(model)
  res$model = model
  res$table = table(model$pred$obs,model$pred$pred)
  if(isDoROC){
    res$ROC = roc(response=model$pred$obs,predictor=model$pred$Patient, thresholds=seq(0.1,1,0.1))
  }
  return(res)
}
presentation=function(res){
  for (n in names(res)){
    if (n!="ROC"){
      print(res[n])
    }else{
      plot.roc(res$ROC,print.auc =T,xlab="Specificity",col="red",axes=T)
    }
  }
}

#Dataset analytics
cor(dataset[,c(0:9)])

set.seed(42)

#cross validation
train_control = trainControl(method  = "repeatedcv",number  = 5, repeats=5,classProbs = TRUE,savePredictions = T)

#Building train dataset
train_set = dataset[,c(0:9)]
YNClassification <- factor(Classification, levels=c(1,2), labels=c("Healthycontrol","Patient" ))
train_set$YNClassification =YNClassification

#Formulas

full_formula = YNClassification~Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1

#Feature Selection

regfit.full = regsubsets(YNClassification~.,dataset[,c(0:9)],nvmax=20,method="exhaustive")
summary(regfit.full)
reg.summary = summary(regfit.full)
reg.summary$adjr2

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
p=which.max(reg.summary$adjr2)
paste("Adjusted RSq Best Number of Variable",p)
points(p,reg.summary$adjr2[p], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
p=which.min(reg.summary$cp)
paste("Cp Best Number of Variable",p)
points(p,reg.summary$cp[p],col="red",cex=2,pch=20)
p=which.min(reg.summary$bic)
paste("Cp Number of Variable",p)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(p,reg.summary$bic[p],col="red",cex=2,pch=20)

opt_formula = YNClassification~Age+BMI+Glucose+Insulin+HOMA+Resistin



# GLM
log=train(full_formula,data=train_set,method="glm", metric = "Accuracy", trControl=train_control,preProcess=c("pca"))
log.res = getResult(log)
presentation(log.res)



#Optimisation
OptLog = train(opt_formula,data=train_set,method="glm", metric = "Accuracy", trControl=train_control,preProcess=c("pca"))
OptLog.res = getResult(OptLog)
presentation(OptLog.res)

#KNN
K_Max = nrow(train_set)/2
knn=train(full_formula,data=train_set,method="knn", tuneGrid=expand.grid(k=1:K_Max),metric = "Accuracy", trControl=train_control,preProcess="pca")
plot(knn)
knn.res = getResult(knn)
presentation(knn.res)

#Optimisation
knn=train(opt_formula,data=train_set,method="knn", tuneGrid=expand.grid(k=1:K_Max),metric = "Accuracy", trControl=train_control,preProcess="pca")
plot(knn)
knn.res = getResult(knn)
presentation(knn.res)

#LDA
#lda2 has tuning parameters
LDA =train(full_formula,data=train_set,method="lda2",tuneGrid=expand.grid(dimen=1:5),metric = "Accuracy", trControl=train_control,preProcess="pca")
plot(LDA)
LDA.res = getResult(LDA)
presentation(LDA.res)

#Optimisation
LDA =train(opt_formula,data=train_set,method="lda2",tuneGrid=expand.grid(dimen=1:5),metric = "Accuracy", trControl=train_control,preProcess="pca")
plot(LDA)
LDA.res = getResult(LDA)
presentation(LDA.res)

#QDA
QDA =train(full_formula,data=train_set,method="qda",metric = "Accuracy", trControl=train_control,preProcess="pca")
QDA.res = getResult(QDA)
presentation(QDA.res)

#Optimisation
QDA =train(opt_formula,data=train_set,method="qda",metric = "Accuracy", trControl=train_control,preProcess="pca")
QDA.res = getResult(QDA)
presentation(QDA.res)

# Decision trees
#grid search
tunegrid <- expand.grid(.mtry=c(1:15))
dtree = train(full_formula,data = train_set,metric = "Accuracy",method = "rf",tunegrid=tunegrid,ntree = 100)
plot(dtree$finalModel)
plot(dtree)
print(dtree)
dtree.res = getResult(dtree,FALSE)
presentation(dtree.res)

#random search
dtree = train(full_formula,data = train_set,metric = "Accuracy",method = "rf",tuneLenght=8,ntree = 100)
plot(dtree$finalModel)
plot(dtree)
print(dtree)
dtree.res = getResult(dtree,FALSE)
presentation(dtree.res)


#Optimisation