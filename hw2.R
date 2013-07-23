########################################################
## Homework 2
## knn classification and kfold cross validation
## Charles Kwong
## 7/22/2013
########################################################

# load classes and iris dataset
library(class)
data(iris)
head(iris)

# ?knn

# 150 rows, decide to do 5 fold cross validation with buckets of 30
nrow(iris)

# set random seed and randomize dataset
set.seed(1337)
idx = sample(nrow(iris))

# create training and testing dataset 
test.1 = iris[idx[1:30],]
train.1 = iris[idx[31:150],]

x_train.1 = train.1[,1:4]
y_train.1 = as.factor(train.1[,5])

x_test.1 = test.1[,1:4]
y_test.1 = as.factor(test.1[,5])

# run knn model and get misclassfication error rate on test set
knn(train=x_train.1, test=x_test.1, cl = y_train.1, k = 5)
knn.model <- knn(train=x_train.1, test=x_test.1, cl = y_train.1, k = 5)
table(y_test.1, knn.model)
sum(y_test.1 != knn.model)/length(y_test.1)

y <- 5
knn.model <- knn(train=train[,-y], test=test[,-y], cl = train[,y], k = 5)
sum(test[,y] != knn.model)/length(test[,y])

error.rate <- data.frame()
error.rate <- rbind(sum(test[,y] != knn.model)/length(test[,y]))

# do k fold cross validation via knn given dataset, dependent variable y (column index), k1 folds, k2 nearest neighbors
# return average error rate across k folds
kfold_knn <- function(dataset, y, k1, k2) {
  # randomize dataset
  idx <- sample(nrow(dataset))
  n <- floor(nrow(dataset)/k1)
  error.rate <- vector()
  # create k folds, if nrows does not divide evenly, put leftover rows in last k fold
  for(i in 1:k1) {
    if(i == k1) {
      test <- dataset[idx[(n*i-n+1):nrow(dataset)],]
      train <- dataset[-idx[(n*i-n+1):nrow(dataset)],]
    }
    else {
      test <- dataset[idx[(n*i-n+1):(n*i)],]
      train <- dataset[-idx[(n*i-n+1):(n*i)],]
    }
    # for each k fold, run model on training set
    knn.model <- knn(train=train[,-y], test=test[,-y], cl = train[,y], k = k2)
    # store error rate 
    error.rate <- append(error.rate,sum(test[,y] != knn.model)/length(test[,y]))
    #print(n*i-n+1)
    #print(n*i)
    #print(nrow(train))
    #print(nrow(test))
    #print(error.rate)
  }
  return(mean(error.rate))
}

########################################################
# use knn to do kfold cross validation 
# arguments, dataset = iris, y dependent variable = 5th column, k folds = 3, k nearest neighbors = 5
kfold_knn(iris,5,3,5)
########################################################