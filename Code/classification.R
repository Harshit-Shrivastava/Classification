library('rpart')
library('rpart.plot')
data = read.table("../Data/page-blocks.data", sep = "")
data = as.matrix(data)
fit =  rpart(data[,11] ~ data[,1:10], data=as.data.frame(data))
rpart.plot(fit)

class.pred <- table(predict(fit, type="class"), data[,11]) #confusion matrix
cerror1 = 1-sum(diag(class.pred))/sum(class.pred) #classification error
cerror1
