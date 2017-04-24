library('rpart')
library('rpart.plot')
dataset = read.table("../Data/page-blocks.data", sep = "")
dataset = as.matrix(dataset)

##experiments start here

bound <- floor((nrow(dataset)/4)*3)         #define % of training and test set

dataset <- dataset[sample(nrow(dataset)), ]           #sample rows 
trainset <- dataset[1:bound, ]              #get training set
testset <- dataset[(bound+1):nrow(dataset), ]    #get test set

model <- rpart(trainset[,11] ~ trainset[,1:10], data = as.data.frame(trainset), minbucket=5)
pred <- predict(model, data = as.data.frame(testset))

##experiments end here


fit =  rpart(data[,11] ~ data[,1:10], data=as.data.frame(data))
rpart.plot(fit)

class.pred <- table(predict(fit, type="class"), data[,11]) #confusion matrix
cerror1 = 1-sum(diag(class.pred))/sum(class.pred) #classification error
cerror1
