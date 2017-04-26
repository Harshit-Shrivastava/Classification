library('rpart')
library('rpart.plot')
dataset = read.table("../Data/page-blocks.data", sep = "")
dataset = as.matrix(dataset)

#dividing the testing and training dataset into half-and-half
bound <- floor((nrow(dataset)/2))
dataset <- dataset[sample(nrow(dataset)), ]           #sample rows 
trainset <- dataset[1:bound, ]              #get training set
testset <- dataset[(bound+1):nrow(dataset), ]    #get test set

#creating the model
model <- rpart(trainset[,11] ~ trainset[,1:10], data = as.data.frame(trainset), minbucket=5)

#plotting the tree, along with attributes and split factors
rpart.plot(model, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE, fallen.leaves = TRUE)

#printing and plotting relative error vs cp and size of tree for pruning
printcp(model)
plotcp(mode)

#testing the model
pred <- predict(model, data = as.data.frame(testset))

#calculating accuracy
accuracy = 1 - mean(pred == testset[ , testset[,11]]) 
print(paste("Accuracy of decision tree on this test set is ", accuracy))

table(pred)