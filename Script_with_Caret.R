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
getAccuracyFromTable = function(model.res){
  table = model.res$table
  healthy = table[1]/(table[1]+table[2])
  patient = table[4]/(table[3]+table[4])
  return(c(healthy,patient))
}
getMaxAccuracy = function(model){
  n = model$method
  n= max(model$results$Accuracy)
  return (n)
}
presentation=function(res){
  for (n in names(res)){
    if (n!="ROC"){
      print(res[n])
    }else{
      plot.roc(res$ROC,print.auc =T,xlab="Specificity",col="red",axes=T,print.thres = "best")
    }
  }
}

#Dataset analytics
cor(dataset[,c(0:9)])

set.seed(42)

#Building train dataset
train_set = dataset[,c(0:9)]
YNClassification <- factor(Classification, levels=c(1,2), labels=c("Healthycontrol","Patient" ))
train_set$YNClassification =YNClassification

#cross validation
seeds = vector(mode = "list", length = nrows(train_set) + 1)
seeds = lapply(seeds, function(x) 1:40)
train_control = trainControl(method  = "repeatedcv",number  = 5,repeats = 5,classProbs = TRUE,savePredictions = T,seeds=seeds,search="grid")
seeds_knn = vector(mode = "list", length = 25 + 1)
seeds_knn = lapply(seeds_knn, function(x) 1:40)#nrows(train_set)/2
train_control_knn = trainControl(method  = "repeatedcv",number  = 5,repeats = 5,classProbs = TRUE,savePredictions = T,seeds=seeds_knn,search="grid")
train_control_random = trainControl(method  = "repeatedcv",number  = 5,repeats = 5,classProbs = TRUE,savePredictions = T,seeds=seeds,search="random")

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

opt_formula = YNClassification~BMI+Glucose+Insulin+HOMA+Resistin

model_list = c()

# GLM
log=train(full_formula,data=train_set,method="glm", metric = "Accuracy", trControl=train_control,preProcess=c("pca"))
log.res = getResult(log)
presentation(log.res)
getAccuracyFromTable(log.res)

#Optimisation
log_opt = train(opt_formula,data=train_set,method="glm", metric = "Accuracy", trControl=train_control,preProcess=c("pca","scale"))
log_opt.res = getResult(log_opt)
presentation(log_opt.res)
getAccuracyFromTable(log_opt.res)

#KNN
K_Max = 40 #nrow(train_set)/2
knn=train(full_formula,data=train_set,method="knn", tuneGrid=expand.grid(k=1:K_Max),metric = "Accuracy", trControl=train_control_knn,preProcess="pca")
plot(knn)
knn.res = getResult(knn)
presentation(knn.res)
getAccuracyFromTable(knn.res)

#Optimisation
knn_opt=train(opt_formula,data=train_set,method="knn", tuneGrid=expand.grid(k=1:K_Max),metric = "Accuracy", trControl=train_control_knn,preProcess=c("pca"))
plot(knn_opt)
knn_opt.res = getResult(knn_opt)
presentation(knn_opt.res)
getAccuracyFromTable(knn_opt.res)

#LDA
#lda2 has tuning parameters
LDA =train(full_formula,data=train_set,method="lda2",tuneGrid=expand.grid(dimen=1:5),metric = "Accuracy", trControl=train_control)
plot(LDA)
LDA.res = getResult(LDA)
presentation(LDA.res)
getAccuracyFromTable(LDA.res)

#Optimisation
LDA_opt =train(opt_formula,data=train_set,method="lda2",tuneGrid=expand.grid(dimen=1:5),metric = "Accuracy", trControl=train_control,preProcess=c("scale","center"))
plot(LDA_opt)
LDA_opt.res = getResult(LDA_opt)
presentation(LDA_opt.res)
getAccuracyFromTable(LDA_opt.res)

#QDA
QDA =train(full_formula,data=train_set,method="qda",metric = "Accuracy", trControl=train_control)
QDA.res = getResult(QDA)
presentation(QDA.res)
getAccuracyFromTable(QDA.res)

#Optimisation
QDA_opt =train(full_formula,data=train_set,method="qda",metric = "Accuracy", trControl=train_control,preProcess=c("pca"))
QDA_opt.res = getResult(QDA_opt)
presentation(QDA_opt.res)
getAccuracyFromTable(QDA_opt.res)

# Random Forest
#grid search
tunegrid <- expand.grid(.mtry=c(1:9))
dtree_gd = train(full_formula,data = train_set,metric = "Accuracy",method = "rf",tuneLenght=30,tunegrid=tunegrid,ntree = 100,trControl=train_control)
plot(dtree_gd$finalModel)
plot(dtree_gd)
print(dtree_gd)
dtree_gd.res = getResult(dtree_gd,FALSE)
presentation(dtree_gd.res)

#random search

dtree_rs = train(full_formula,data = train_set,metric = "Accuracy",method = "rf",tuneLenght=30,tunegrid=tunegrid,ntree = 100,trControl=train_control_random)
plot(dtree_rs$finalModel)
plot(dtree_rs)
print(dtree_rs)
dtree_rs.res = getResult(dtree_rs,FALSE)
presentation(dtree_rs.res)

accuracy_list = c(getMaxAccuracy(log_opt),getMaxAccuracy(knn_opt),getMaxAccuracy(LDA_opt),getMaxAccuracy(QDA_opt),getMaxAccuracy(dtree_gd),getMaxAccuracy(dtree_rs))
model_list=c("glm","knn","LDA","QDA","rf_gs","rf_rs")
df_accuracy = data.frame(model_list,accuracy_list)
plot(df_accuracy)
df_accuracy
