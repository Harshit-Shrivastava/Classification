library('rpart')
library('rpart.plot')
dataset = read.table("../Data/page-blocks.data", sep = "")
dataset = as.data.frame(dataset)
dataset <- subset(dataset, select = -c(V9) )
#dividing the testing and training dataset into half-and-half
bound <- floor((nrow(dataset)*0.8))
dataset <- dataset[sample(nrow(dataset)), ]           #sample rows 
trainset <- dataset[1:bound, ]              #get training set
testset <- dataset[(bound+1):nrow(dataset), ]    #get test set

#creating the model
model <- rpart(trainset$V11 ~ ., data = as.data.frame(trainset), method="class", minbucket=5)

#plotting the tree, along with attributes and split factors
rpart.plot(model, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE, fallen.leaves = TRUE)


#printing and plotting relative error vs cp and size of tree for pruning
printcp(model)
plotcp(model)

class.pred <- table(predict(model, testset ,type="class"), testset$V11)
cerror0 = 1-sum(diag(class.pred))/sum(class.pred)
cerror0
class.pred