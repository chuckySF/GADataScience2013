########################################################
## Homework 3
## naive bayes classification and kfold cross validation
## Charles Kwong
## 7/22/2013
########################################################

# load classes and iris dataset
library(e1071)
data(iris)
head(iris)

# ?naiveBayes

# do a test run on naive bayes
model <- naiveBayes(Species ~ ., iris)
model

# ?predict
table(predict=predict(model, iris[,-5]), actual=iris[,5])

# do a test run on naive bayes with k fold cross validation

nrow(iris)
# 150 rows, decide to do 5 fold cross validation with buckets of 30

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

# run naive bayes model 
model.1 <- naiveBayes(y_train.1 ~ ., x_train.1)
table(predict=predict(model.1, x_train.1), actual=y_train.1)

# compare test results with prediction
y_test.1==predict(model.1, x_test.1)

# how many were correct
sum(y_test.1==predict(model.1, x_test.1))

# percentage correct
sum(y_test.1==predict(model.1, x_test.1))/length(y_test.1)

# misclassification rate
1-sum(y_test.1==predict(model.1, x_test.1))/length(y_test.1)

# do k fold cross validation via naive bayes given dataset, dependent variable y (column index), k folds
# return average error rate across k folds
kfold_naive_bayes <- function(dataset, y, k) {
  # randomize dataset
  idx <- sample(nrow(dataset))
  n <- floor(nrow(dataset)/k)
  error.rate <- vector()
  # create k folds, if nrows does not divide evenly, put leftover rows in last k fold
  for(i in 1:k) {
    if(i == k) {
      test <- dataset[idx[(n*i-n+1):nrow(dataset)],]
      train <- dataset[-idx[(n*i-n+1):nrow(dataset)],]
    }
    else {
      test <- dataset[idx[(n*i-n+1):(n*i)],]
      train <- dataset[-idx[(n*i-n+1):(n*i)],]
    }
	# for each k fold, run model on training set
    model <- naiveBayes(train[,y] ~ ., train[,-y])
    # store error rate 
    error.rate <- append(error.rate,1-sum(test[,y]==predict(model, test[,-y]))/length(test[,y]))
    #print(n*i-n+1)
    #print(n*i)
    #print(nrow(train))
    #print(nrow(test))
	#print(error.rate)
  }
  return(mean(error.rate))
}

########################################################
# use naive bayes to do kfold cross validation 
# arguments, dataset = iris, y dependent variable = 5th column, k folds = 3
kfold_naive_bayes(iris,5,3)
########################################################