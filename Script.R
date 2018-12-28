library(pROC)
library("e1071")
library(class)
library(tree)
library(MASS)

### Exploratory analysis of the data set
dataset=read.table(file = 'dataR2.csv',header = T, sep=",")
attach(dataset)
summary(dataset)
plot(dataset)
cor(dataset)

### Logistic regression
YNClassification <- factor(Classification, levels=c(1,2), labels=c("Healthy control","Patient" ))
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
log2.probs=predict(log2.fit,newdata = test,type="response")
log2.pred=rep("Healthy control",nrow(test))
log2.pred[log2.probs>0.5]="Patient"

test$YNClassification <- factor(test$Classification, levels=c(1,2), labels=c("Healthy control","Patient" ))
#Confusion matrix
table(log2.pred,test$YNClassification)
#Accuracy
mean(log2.pred==test$YNClassification)
mean(log2.pred!=test$YNClassification)

# Model selection for logistic regression : 
# Use model selection methods to select a pertinent 
# subset of features for the logistic regression classifier.
# Draw the Error - rate versus flexibility curve in order 
# to choose the best level of flexibility

### KNN


#TODO: standarize data ?

train.X= as.matrix(train[,-c(11,10)])
test.X= as.matrix(test[,-c(11,10)])
train.Y=YNClassification[train_bool]
test.Y=YNClassification[!train_bool]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)

#Confusion matrix
table(knn.pred,YNClassification[!train_bool])
#Accuracy
Accuracy=mean(knn.pred==test.Y)
test.error=mean(knn.pred!=test.Y)
test.error=c()
for(i in c(1,10,50,100,150,200,250))
{
  set.seed(1)
  knn.pred=knn(train.X,test.X,train.Y,k=i)
  test.error=c(test.error,mean(knn.pred!=test.Y))
}
plot(c(1,10,50,100,150,200,250),test.error)

### LDA

lda.fit=lda(YNClassification~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1)
lda.pred=predict(lda.fit)
lda.pred$posterior[,1]


#ROC curve
ROC.lda=roc(YNClassification,lda.pred$posterior[,1],levels=c("Healthy control","Patient"), thresholds=seq(0.1,1,0.1))
plot.roc(ROC.lda,print.auc =T,xlab="Specificity",col="red",axes=T)


### QDA

qda.fit=qda(YNClassification~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1)
qda.pred=predict(qda.fit)
qda.pred$posterior[,1]

#ROC curve
ROC.qda=roc(YNClassification,qda.pred$posterior[,1],levels=c("Healthy control","Patient"), thresholds=seq(0.1,1,0.1))
plot.roc(ROC.qda,print.auc =T,xlab="Specificity",col="red",axes=T)



### Decision trees

tree.dataset=tree(YNClassification ~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,dataset ,subset=train_ind)
summary(tree.dataset)
plot(tree.dataset)
text(tree.dataset,pretty=0)


### Support Vector Machine

set.seed(1)
