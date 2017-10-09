######
# kNN
######

# Do not clear your workspace

# load required libraries
require(class) # for kNN classifier
require(caret) # for createDataPartition, train, predict
require(randomForest) # for random forest classifier
require(MASS) # for neural net classifier

# set seed to ensure reproducibility
set.seed(100)

# load in-built dataset
data(iris)

# normalize all predictors, i.e., all but last column species
iris[, -ncol(iris)] <- scale(iris[, -ncol(iris)])

# split the data into training and test sets 70/30 split
# take a partition of the indexes then use the index vectors to subset
###
trainIdx <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
# those Idxs in original data but not in trainIdx will form testIdx
###
testIdx <- setdiff(1:150,trainIdx )
# subset the original dataset with trainIdx and testIdx, use all but last column
train <- iris[trainIdx, -ncol(iris)]
test <- iris[testIdx, -ncol(iris)]
# create a factor for the training data class variable
###
trainwo<-iris[trainIdx,]
cl <- factor(trainwo$Species)
# use random forest from randomForest package to predict
###
testwo<-iris[testIdx,]
RFmodel <- train(train,cl, method = "rf")
RFpreds <- predict(RFmodel,newdata = test)
# create contingency table of predictions against ground truth
###
clt<-factor(testwo$Species)
table(RFpreds, clt)
# table(RFpreds, factor([, ]))

# use neural network from MASS package to predict
###
NNmodel <- train(train,cl, method = "nnet")
NNpreds <- predict(NNmodel,newdata = test )

# create contingency table of predictions against ground truth
###
table(NNpreds, clt)

# use knn from class package to predict, use 3 nearest neighbors
###
knnPreds <- knn(train,test,cl,k = 3 , prob = TRUE)

# create contingency table of predictions against ground truth
###
table(knnPreds, clt)

# implement myknn with manhattan distance, majority vote and 
# resolving ties by priority setosa > versicolor > virginica
myknn <- function(train, test, cl, k)
{
    classes <- vector()
    for(i in 1:nrow(test))
    {
        dists <- vector()
        for(j in 1:nrow(train))
        {
            # implement manhattan distance calculation without using the
            # dist function
            ###
            differences <- abs(test[i,1]-train[j,1]) + abs(test[i,2]-train[j,2])+ abs(test[i,3]-train[j,3]) + abs(test[i,4]-train[j,4]) 
            dists <- c(dists,differences)
        }
        # implement majority vote and resolving ties by priority to assign class
        # functions order, max, which.max and table could be useful
        ###
        odists<-order(dists,decreasing = FALSE)
        odists<-odists[1:k]
        t <- table(cl[odists])
        h <- as.vector(t)
        maxClassIndex <- which( h == max(h) )
        
          if(maxClassIndex[1]==1){
            classes[i]<-'setosa'
          }
          if(maxClassIndex[1]==2){
            classes[i]<-'versicolor'
          }
          if(maxClassIndex[1]==3){
            classes[i]<-'virginica'
          }
    }
    return(factor(classes))
}

# predict using your implemented function
myPreds <- myknn(train, test, cl, k = 3)

# create contingency table of predictions against ground truth
###
table(myPreds, clt)

# compare with the knn from class package
table(myPreds, knnPreds)

