## KNN Modeling 
## Too many ties in data, data needs to be more detailed for KNN classification 
library(class)
library(MASS)
library(caret)
library(dplyr)

# need to source the data 
source("Import_and_Explore.R")

# dev.off()

######### Preprocessing Data ######### 

str(nu_clustering_d7)

#chceck for NAs 
sapply(nu_clustering_d7, function(y) sum(length(which(is.na(y)))))
#turnall of the NAs into 0 
nu_clustering_d7[is.na(nu_clustering_d7)] <- 0

# get modeling data 
nu.knn.d7.modeling <- nu_clustering_d7[, c(#'amplitude_id'
                                            'd14_retanined_status'
                                           ,'d7_avgmyturrets'
                                           ,'d7_avgtheirturrets'
                                           ,'d7_hero_play_count'
                                           ,'d7_hero_win_count'
                                           ,'d7_qpmr_modes_finished_count'
                                           ,'d7_win_count'
                                           ,'d7_accountlevel'
                                           ,'d7_days_played_count'
                                           ,'d7_avg_minutes_played_per_day'
                                           ,'d7_glory_earned_total'
                                           ,'d7_mmrbucket')]
#check 
str(nu.knn.d7.modeling)

# sample down while working 
nu.knn.d7.modeling.samp.10k <-sample_n(nu.knn.d7.modeling, 10000) 

# partition the data for training and testing
set.seed(1234)
inTrain  <- createDataPartition(nu.knn.d7.modeling.samp.10k$d14_retanined_status
                                ,p = .8
                                ,list = FALSE
                                ,times = 1)

#apply actual partictions
training <- nu.knn.d7.modeling.samp.10k[ inTrain,]  
testing <- nu.knn.d7.modeling.samp.10k[-inTrain,]  

# Training set DV and IVs 
train.knn <- nu.knn.d7.modeling.samp.10k[1:5000, 3:ncol(training)]
train.class <- nu.knn.d7.modeling.samp.10k[1:5000, 1]

# Test set DV and IVs 
test.knn <- nu.knn.d7.modeling.samp.10k[5001:6000, 3:ncol(training)]  
test.class <- nu.knn.d7.modeling.samp.10k[5001:6000, 1]

# remove the amplitude_id before modeling
# training <- training[, -c(1,2)]  #remove the amplitude_id and d14_retanined_status

# get modeling vars 
knn.vars <- colnames(training)[2:ncol(training)]     

# center and scale data 
training.matrix  <- scale(training[,knn.vars])  
# check 
head(training.matrix)

# gather annotated output, used for unscaling later 
training.matrix.center <- attr(training.matrix,  "scaled:center")  
training.matrix.scale <- attr(training.matrix, "scaled:scale")


######### KNN Functions ######### 

## Helper function that returns the indices of a specific fold in cross-validation 
GetFoldIndex <- function(tt,n.folds){
  # This function creates a vector of indexes that corresponds to n.folds 
  # equal size sub-samples taken from tt
  # Args:
  # tt: data set with explanatory variables and class variable
  # n.folds: Number of folds (number of sub-samples)
  
  n <- dim(tt)[1] # Number of observations
  
  # vector of folds lables
  folds <- rep(1:n.folds,each=floor(n/n.folds)) 
  remainder <- n-length(folds)
  
  # number of folds might not be a multiple of total number of obs. so
  # we assign remaining obs to a fold systematically: i.e. 1st goes to fold 1, etc
  if(remainder>0){
    folds <- c(folds,1:remainder)
  }
  
  # we finally permute indexes
  folds <- sample(folds)
  
  return(folds)
}


# KNN cross validation 
MiscErrorKNN <- function(X,responseY,m,n){
  # Args:
  # X: dataset with explanatory variables 
  # responseY : lables
  # m: max value for nearest neighbor
  # n: Number of folds
  
  error.cv <- list()
  #  Add index of sub-samples for n-fold validation
  
  data.set <-  data.frame(X,responseY,Fold=GetFoldIndex(X,n.folds = n)) 
  
  #  100% observations plus vector of subsamples
  for(i in 1:n){
    
    # Training data
    train.set <- subset(data.set, Fold != i)[,colnames(X)]
    
    # Test data
    test.set <- subset(data.set, Fold == i)[,colnames(X)]
    
    # Vector of classes
    class.train <- subset(data.set,Fold != i)[,"responseY"]
    class.test <- subset(data.set,Fold == i)[,"responseY"]
    
    # For these given samples fit k-NN model for several values of k
    knn.error <- vector()  # initialize vector
    for (j in 1:m){  # m: Maximum number of values of k
      model.knn <- knn(train = train.set,
                       test = test.set,
                       cl = class.train,
                       k=j,
                       prob=T)  # Fit model
      error <- table(model.knn,class.test)
      # Compute Error
      knn.error[j] <- (error[1,2] + error[2,1])/sum(error)
      #return(knn.error)
    }
    error.cv[[i]] <- knn.error
  }
  return(error.cv)
}


######### 30 Min Modeling 200 Min ######### 

# # Apply function 
# MiscErrorKNN(predictorX_data_churn30_200_knn, churn_data_churn30_200_knn, 50, 2)
# # two inputs are created in data_churn30_200.R 
# 
# #  Compute Errors using up to k=30 nearest neighbors 10 times   
# CrossValid <- MiscErrorKNN(X = X,     # Explanaory Variables
#                            responseY = responseY,  # labels
#                            m=50,                  # Maximum value of k, for k-NN
#                            n=10)                  # Number of folds (subsamples)
# class(CrossValid)
# names(CrossValid) <- paste("Sample",1:10) # Assign names


######### D7 Modeling D14 ######### 

# Fit models up to k = 50 NN, 10 - fold cross validation 
CrossValid <- MiscErrorKNN(train.knn, train.class, 50, 10)

# Plot error curves
op <- par(mfrow = c(2,1)) # chance graphical device to include 2 plots
# 2 rows, 1 column
matplot(data.frame(CrossValid), type = "l", lty=1,
        ylab = "Error Rate", 
        xlab = "k",
        main = "10-Fold Misclassification Test Error")

# To get the estimated value of k we average over estimated  error from
# the 10 estimated models
mean.error <- apply(data.frame(CrossValid),1,mean)  # Get the mean for each k
lines(mean.error,lwd=2)

boxplot(t(data.frame(CrossValid)))  # Box plot of errors
lines(1:50,mean.error, type = "l",lwd=2, col = "red") #  Add mean
title(paste("10-Fold Avg. Cross Validation Error: Local minimum: k=",which.min(mean.error)), 
      xlab = "k (No. of Nearest Neighbors)", 
      ylab = "Misclassification Error")

# Place lines to indicate minimum average error and value of k
abline(h=mean.error[which.min(mean.error)],
       v = which.min(mean.error),
       col = "gray",lty = 2)
points(which.min(mean.error),mean.error[which.min(mean.error)],
       pch = 19,col = "blue",cex=1)

# add legend
legend("topright",c("Avg. Error"),lty = 1,col="red",lwd=2)

# Fit best model according to CV-error curves 
# K = 9-11 (range of results from multiple trials)
model1.knn <- knn(train = train.knn,
                  test = test.knn, 
                  cl = train.class, 
                  k = 9,
                  prob = T)

# Sensitivity and Specificity of best model 
model1.knn.cm <- 
confusionMatrix(data = model1.knn[1:1000]
                ,test.class
                ,positive = "churned")



