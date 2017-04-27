#=========================================
#Data reading
data = read.table("../Data/page-blocks.data", sep = "")
data = as.matrix(data)
data = data[,-9] #removing the correlated feature

#=========================================
#Data pre-processing
divide_dataset <- function(df){
  bound <- floor(nrow(df)*0.8)         
  df <- df[sample(nrow(df)), ]           
  df.train <- df[1:bound, ]              
  df.test <- df[(bound+1):nrow(df), ]   
  return(list(train=df.train, test=df.test))
}

processData <- function(X, case) {
  if(case == 1) {
    ###case-1: random sampling of data
    dataset = divide_dataset(X)
    train = dataset$train
    test = dataset$test
    print(dim(train))
    print(dim(test))
  } else {
    ###case-2: stratified sampling of data
    X1 = data[data[,10]==1,1:10]
    X2 = data[data[,10]==2,1:10]
    X3 = data[data[,10]==3,1:10]
    X4 = data[data[,10]==4,1:10]
    X5 = data[data[,10]==5,1:10]
    if(case == 3) { 
      ###case-3: over sample data to increase the record count of less occupied classes
      X2 = do.call(rbind, replicate(12, X2, simplify=FALSE))
      X3 = do.call(rbind, replicate(120, X3, simplify=FALSE))
      X4 = do.call(rbind, replicate(35, X4, simplify=FALSE))
      X5 = do.call(rbind, replicate(30, X5, simplify=FALSE))
    }
    a = divide_dataset(X1)
    X1.train = a$train
    X1.test = a$test
    
    a = divide_dataset(X2)
    X2.train = a$train
    X2.test = a$test
    
    a = divide_dataset(X3)
    X3.train = a$train
    X3.test = a$test
    
    a = divide_dataset(X4)
    X4.train = a$train
    X4.test = a$test
    
    a = divide_dataset(X5)
    X5.train = a$train
    X5.test = a$test
    
    train = rbind(cbind(X1.train, rep(1, nrow(X1.train)))
                       , cbind(X2.train, rep(2, nrow(X2.train)))
                       , cbind(X3.train, rep(3, nrow(X3.train)))
                       , cbind(X4.train, rep(4, nrow(X4.train)))
                       , cbind(X5.train, rep(5, nrow(X5.train))))
    
    test = rbind(cbind(X1.test, rep(1, nrow(X1.test)))
                      , cbind(X2.test, rep(2, nrow(X2.test)))
                      , cbind(X3.test, rep(3, nrow(X3.test)))
                      , cbind(X4.test, rep(4, nrow(X4.test)))
                      , cbind(X5.test, rep(5, nrow(X5.test))))
  }
  return(list(train=train, test=test))
}

data_processed = processData(data, 3) #give the case number as per the required data pre-processing methods
train.data = data_processed$train
test.data = data_processed$test

#=======================================
#Classification Algorithm

#learning parameters and class prior probabilities
mu_mat = matrix(0,nrow=5,ncol=9)
sigma_mat = matrix(0,nrow=5,ncol=9)
prior = rep(0,5)
for(c in 1:5) {
  class_data = train.data[train.data[,10]==c,1:9]
  mu = colMeans(class_data)
  mu_mat[c,] = mu
  mu_rep = matrix(rep(mu, each = nrow(class_data)), nrow(class_data))
  sq_diff = (class_data - mu_rep)^2
  sigma_mat[c,] = sqrt(colMeans(sq_diff))
  prior[c] = nrow(class_data)/nrow(train.data)
}

#returns predicted classes and classification error
classification_error_naive_bayes <- function(A) {
  yhat = rep(0,nrow(A));	# the estimated class for each example
  for (i in 1:nrow(A)) {  # for each example
    pc = prior;	     # begin with prior dist
    o = A[i,];	     # ith observation
    for (c in 1:5) {  # for each class
      for (j in 1:9) { # for each feature
        a = ((o[j] - mu_mat[c,j])^2)/(2*sigma_mat[c,j]^2)
        prob = (1/(sqrt(2*pi)*sigma_mat[c,j]))*exp(-a)
        pc[c] = pc[c] * prob;      # accumulate prob in pc[c]
      }
    }
    yhat[i] = which.max(pc);  # take maximizing class
  }
  error_rate =  sum(yhat != A[,10])/nrow(A)
  return(list(error_rate=error_rate, yhat=yhat))
}

class_model = classification_error_naive_bayes(train.data)
yhat.train = class_model$yhat
train.error = class_model$error_rate

class_model_test = classification_error_naive_bayes(test.data)
yhat.test = class_model$yhat
test.error = class_model_test$error_rate

#confusion matrix
conf_mat <- function(y_pred, y_act) {
  A = matrix(0, nrow = 5, ncol = 5)
  for(i in 1:length(y_pred)) {
    r = y_act[i]
    c = y_pred[i]
    A[r, c] = A[r, c] + 1
  }
  return(A)
}

#class distribution of data
table(train.data[,10])
table(test.data[,10])

#error rates on training and test sets
train.error
test.error

#confusion matrices on both training and test sets
confusion_matrix_train = conf_mat(yhat.train, train.data[,10])
confusion_matrix_test = conf_mat(yhat.test, test.data[,10])
confusion_matrix_train
confusion_matrix_test