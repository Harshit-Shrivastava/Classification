data = read.table("../Data/page-blocks.data", sep = "")
data = as.matrix(data)
bound <- floor(nrow(data)*0.5)
data <- data[sample(nrow(data)), ]   
train.data = data[1:bound,]
test.data = data[(bound+1):nrow(data),]
table(train.data[,11])
table(test.data[,11])
y = unique(data[,11])

mu_mat = matrix(0,nrow=5,ncol=10)
sigma_mat = matrix(0,nrow=5,ncol=10)
prior = rep(0,5)
for(c in 1:5) {
  class_data = train.data[train.data[,11]==c,1:10]
  mu = colMeans(class_data)
  mu_mat[c,] = mu
  mu_rep = matrix(rep(mu, each = nrow(class_data)), nrow(class_data))
  sq_diff = (class_data - mu_rep)^2
  sigma_mat[c,] = sqrt(colMeans(sq_diff))
  prior[c] = nrow(class_data)/nrow(train.data)
}

classification_error_naive_bayes <- function(A) {
  yhat = rep(0,nrow(A));	# the estimated class for each example
  for (i in 1:nrow(A)) {  # for each example
    pc = prior;	     # begin with prior dist
    o = A[i,];	     # ith observation
    for (c in 1:5) {  # for each class
      for (j in 1:10) { # for each feature
        a = ((o[j] - mu_mat[c,j])^2)/(2*sigma_mat[c,j]^2)
        prob = (1/(sqrt(2*pi)*sigma_mat[c,j]))*exp(-a)
        pc[c] = pc[c] * prob;      # accumulate prob in pc[c]
      }
    }
    yhat[i] = which.max(pc);  # take maximizing class
  }
  error_rate =  sum(yhat != A[,11])/nrow(A)
  return(list(error_rate=error_rate, yhat=yhat))
}

class_model = classification_error_naive_bayes(train.data)
yhat.train = class_model$yhat
train.error = class_model$error_rate

class_model_test = classification_error_naive_bayes(test.data)
yhat.test = class_model$yhat
test.error = class_model_test$error_rate

train.error
test.error


conf_mat <- function(y_pred, y_act) {
  A = matrix(0, nrow = 5, ncol = 5)
  for(i in 1:length(y_pred)) {
    r = y_act[i]
    c = y_pred[i]
    A[r, c] = A[r, c] + 1
  }
  return(A)
}

confusion_matrix_train = conf_mat(yhat.train, train.data[,11])
confusion_matrix_train

confusion_matrix_test = conf_mat(yhat.test, test.data[,11])
confusion_matrix_test
#==============================================
#library(e1071)
#model <- naiveBayes(train.data[,11] ~ ., data = train.data)
#class(model)
#summary(model)
#print(model)

#preds <- predict(model, newdata = as.data.frame(train.data))
#conf_matrix <- table(preds, as.data.frame(train.data[,11]))
#conf_matrix
