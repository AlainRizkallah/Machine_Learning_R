library(pROC)
library("e1071")
library(class)
library(tree)
library(MASS)
library(leaps)


### Exploratory analysis of the data set
dataset=read.table(file = 'dataR2.csv',header = T, sep=",")
attach(dataset)
summary(dataset)
plot(dataset)
#Correlation
cor(dataset[,c(0:9)])


#Redefine the variable to predict
YNClassification <- factor(Classification, levels=c(1,2), labels=c("Healthy control","Patient" ))
sum(YNClassification=="Patient")
sum(YNClassification=="Healthy control")


### Logistic regression
log.fit=glm(YNClassification~Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,data=dataset,family = binomial)
summary(log.fit)


log.probs=predict(log.fit,type="response")
log.probs[1:10] # To visualize the first 10 values.
contrasts(YNClassification) 
log.pred=rep("Healthy control",nrow(dataset))
log.pred[log.probs>0.5]="Patient"

#Confusion matrix
table(log.pred,YNClassification)
#Accuracy
mean(log.pred==YNClassification)

#Optimisation

logOpt.fit=glm(YNClassification~Age+BMI+Glucose+Insulin+Leptin+Resistin+MCP.1,data=dataset,family = binomial)

logOpt.probs=predict(logOpt.fit,type="response")
logOpt.pred=rep("Healthy control",nrow(dataset))
logOpt.pred[logOpt.probs>0.5]="Patient"

#Confusion matrix
table(logOpt.pred,YNClassification)
#Accuracy
mean(logOpt.pred==YNClassification)


# Using a train and a test set

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dataset))

set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
train <- dataset[train_ind, ]
train_bool = rep(FALSE,nrow(dataset))
train_bool[train_ind]=TRUE
test <- dataset[-train_ind, ]

log2.fit=glm(YNClassification ~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,data=dataset,family = binomial,subset=train_bool)

summary(log2.fit)
log2.probs=predict(log2.fit,newdata = test,type="response",probability = TRUE)
log2.pred=rep("Healthy control",nrow(test))
log2.pred[log2.probs>0.5]="Patient"

test$YNClassification <- factor(test$Classification, levels=c(1,2), labels=c("Healthy control","Patient" ))
#Confusion matrix
table(log2.pred,test$YNClassification)
#Accuracy
mean(log2.pred==test$YNClassification)

#mean(log2.pred!=test$YNClassification)
ROC.log=roc(YNClassification,log.probs,levels=c("Healthy control","Patient"), thresholds=seq(0.1,1,0.1))
plot.roc(ROC.log,print.auc =T,xlab="Specificity",col="red",axes=T)

# Model selection for logistic regression : 
# Use model selection methods to select a pertinent 
# subset of features for the logistic regression classifier.
# Draw the Error - rate versus flexibility curve in order 
# to choose the best level of flexibility

### KNN

#TODO: standarize data ?

train.X= as.matrix(train[,c(0:9)])
test.X= as.matrix(test[,c(0:9)])
train.Y=YNClassification[train_bool]
test.Y=YNClassification[!train_bool]
set.seed(1)

test.error=c()
NB_MOTIFS = 87
sequence = seq(1,NB_MOTIFS,NB_MOTIFS/6) #seq(40,50,1)
for(i in sequence)
{
  set.seed(1)
  knn.pred=knn(train.X,test.X,train.Y,k=i)
  test.error=c(test.error,mean(knn.pred!=test.Y))
}
plot(sequence,test.error,xlab="Nombre de motifs")

KOpt = 41#2
knn.pred=knn(train.X,test.X,train.Y,k=KOpt)

#Confusion matrix
table(knn.pred,YNClassification[!train_bool])
#Accuracy
mean(knn.pred==test.Y)
test.error=mean(knn.pred!=test.Y)
test.error




### LDA

lda.fit=lda(YNClassification~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,data=dataset,subset = train_bool)
lda.pred=predict(lda.fit,newdata = test,type="response")


par(mfrow=c(1,2))
TPR = c()
TNR = c()
accList = c()
alphaList = seq(0.1,1,0.01)
for(alpha in alphaList){
  #crée un vecteur  qui contient la valeur prédite pour une aleur donnée (initialisé à R)
  lda.pred.class = rep("Patient",nrow(test))
  #Modifie le seuil de probabilité pour prédire G
  #Change si la probabilité d'obtenir G est >alpha (O.5 par défaut)
  lda.pred.class[lda.pred$posterior[,1]>alpha]="Healthy control"
  #sensitivity ou TPR (True positive Rate)
  sensitivity = sum(lda.pred.class=="Healthy control" & test$YNClassification=="Healthy control")/sum(test$YNClassification=="Healthy control")
  TPR = c(TPR,sensitivity)
  #1-specificity ou TNR (True negative Rate)
  specificity = sum(lda.pred.class=="Patient" & test$YNClassification=="Patient")/sum(test$YNClassification=="Patient")
  TNR = c(TNR,specificity)
  accList = c(accList,mean(lda.pred.class==test$YNClassification))
}

plot(TNR,TPR,type="b",ylim=c(0,1),xlim = c(1,0))
plot(alphaList,accList,type="b")
#ROC curve
lda.pred.class = rep("Patient",nrow(test))
ROC.lda=roc(test$YNClassification,lda.pred$posterior[,1],levels=c("Healthy control","Patient"), thresholds=seq(0.1,1,0.1))
plot.roc(ROC.lda,print.auc =T,xlab="Specificity",col="red",axes=T)


lda.pred.class = rep("Patient",nrow(test))
lda.pred.class[lda.pred$posterior[,1]>0.4]="Healthy control"
#Confusion matrix
table(lda.pred.class,test$YNClassification)
#Accuracy
mean(lda.pred.class==test.Y)


### QDA

qda.fit=qda(YNClassification~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,subset = train_bool)
qda.pred=predict(qda.fit,newdata = test,type="response")

par(mfrow=c(1,2))
TPR = c()
TNR = c()
accList = c()
alphaList = seq(0.1,1,0.01)
for(alpha in alphaList){
  #crée un vecteur  qui contient la valeur prédite pour une aleur donnée (initialisé à R)
  qda.pred.class = rep("Patient",nrow(test))
  #Modifie le seuil de probabilité pour prédire G
  #Change si la probabilité d'obtenir G est >alpha (O.5 par défaut)
  qda.pred.class[qda.pred$posterior[,1]>alpha]="Healthy control"
  #sensitivity ou TPR (True positive Rate)
  sensitivity = sum(qda.pred.class=="x²Healthy control" & test$YNClassification=="Healthy control")/sum(test$YNClassification=="Healthy control")
  TPR = c(TPR,sensitivity)
  #1-specificity ou TNR (True negative Rate)
  specificity = sum(qda.pred.class=="Patient" & test$YNClassification=="Patient")/sum(test$YNClassification=="Patient")
  TNR = c(TNR,specificity)
  accList = c(accList,mean(qda.pred.class==test$YNClassification))
}
#plot(TNR,TPR,type="b",ylim=c(0,1),xlim = c(1,0))
plot(alphaList,accList,type="b")

qda.pred.class = rep("Patient",nrow(test))
qda.pred.class[qda.pred$posterior[,1]>0.9]="Healthy control"

#Confusion matrix
table(qda.pred.class,test$YNClassification)
#Accuracy
mean(qda.pred.class==test$YNClassification)

#ROC curve
ROC.qda=roc(YNClassification,qda.pred$posterior[,1],levels=c("Healthy control","Patient"), thresholds=seq(0.1,1,0.1))
plot.roc(ROC.qda,print.auc =T,xlab="Specificity",col="red",axes=T)




### Decision trees

tree.dataset=tree(YNClassification ~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,dataset ,subset=train_ind)
summary(tree.dataset)
plot(tree.dataset)
text(tree.dataset,pretty=0)

dataset.test=dataset[-train_ind,]
dataset.train=dataset[train_ind,]
YNClassification.test=YNClassification[-train_ind]
tree.pred=predict(tree.dataset, dataset.test,type="class")
table(tree.pred,YNClassification.test)

##Cross validation
cv.dataset=cv.tree(tree.dataset, FUN=prune.misclass, K=5)
plot(cv.dataset$size ,cv.dataset$dev ,type='b')

prune.dataset=prune.misclass(tree.dataset, best=3)
summary(prune.dataset)
plot(prune.dataset)
text(prune.dataset,pretty=0)
tree.pred=predict(prune.dataset,dataset.test, type="class")
table(tree.pred,YNClassification.test)

plot(tree.pred)
#Confusion matrix

#Accuracy
mean(tree.dataset[["y"]]==test$YNClassification)



### Support Vector Machine

set.seed(1)

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

