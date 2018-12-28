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
table(log.pred,YNClassification)
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
table(log2.pred,test$YNClassification)
mean(log2.pred==test$YNClassification) 
mean(log2.pred!=test$YNClassification)

# Model selection for logistic regression : 
# Use model selection methods to select a pertinent 
# subset of features for the logistic regression classifier.
# Draw the Error - rate versus flexibility curve in order 
# to choose the best level of flexibility

### KNN

library(class)

### LDA


### QDA


### Decision trees


### Support Vector Machine


