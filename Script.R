#First Script

### Exploratory analysis of the data set

dataset=read.table(file = 'dataR2.csv',header = T, sep=",")
attach(dataset)
summary(dataset)
plot(dataset)
cor(dataset)

### Logistic regression
YNClassification <- factor(Classification, levels=c(1,2), labels=c("Healthy controls","Patients" ))
log.fit=glm(YNClassification~Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1,data=dataset,family = binomial)
summary(log.fit)

### KNN

### LDA
### QDA
### Decision trees
### Support Vector Machine
