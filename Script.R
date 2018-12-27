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

# Use a train and a test set ? 

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


