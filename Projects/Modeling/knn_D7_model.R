### KNN Model Predictions and Performance ### 

library(caret)
library(pROC)

# need to source the data 
source("Import_and_Explore.R")


######### Preprocessing Data ######### 

#chceck for NAs 
sapply(nu_clustering_d7, function(y) sum(length(which(is.na(y)))))
#turnall of the NAs into 0 
nu_clustering_d7[is.na(nu_clustering_d7)] <- 0

# get modeling data 
nu.knn.d7.modeling <- nu_clustering_d7[, c('amplitude_id'
                                           ,'d14_retanined_status'
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

# sample down while working 
nu.knn.d7.modeling.samp.10k <-sample_n(nu.knn.d7.modeling, 10000) 

# create separate version of the modeling data for use with KNN 
forKNN <- nu.knn.d7.modeling.samp.10k

# partition the data for training and testing
set.seed(1234)
inTrain  <- createDataPartition(forKNN$d14_retanined_status
                                ,p = .8
                                ,list = FALSE
                                ,times = 1)

#apply actual partictions
training <- nu.knn.d7.modeling.samp.10k[ inTrain,] 
testing <- nu.knn.d7.modeling.samp.10k[-inTrain,]

nrow(training)
nrow(testing)

# tuneLength resampled hardcoded 
# set resampling parameters
fitControl <- trainControl(method = "repeatedcv"
                           ,number = 10                        # 10 fold CV
                           ,repeats = 3                        # repeated x times
                           ,summaryFunction = twoClassSummary	# Use AUC to pick the best model
                           ,classProbs = TRUE
                           ,allowParallel = TRUE) 

set.seed(1234)
knnFit1 <- train(d14_retanined_status ~ . - amplitude_id
                 ,data = training
                 ,method = "knn"
                 ,metric = "ROC"
                 ,trControl = fitControl
                 ,preProcess = c("center", "scale") #KNN requires standardized values
                 ,tuneLength = 5) # number of K neighbors 

# Make predictions using the test data set
# get predicted classes 
testing$knn.pred <- predict(knnFit1
                            ,testing
                            ,type = "raw") # raw returns the predicted class 

# get predicted class probabilities  
knn.prob <- predict(knnFit1
                    ,testing
                    ,type = "prob")   

#Look at the confusion matrix  
knn.testing.cm <- 
  confusionMatrix(data = testing$knn.pred
                  ,testing$d14_retanined_status
                  ,positive = "churned") 
knn.testing.cm

# Plotting the Resampling Profile
# to examine relationship between the estimates of performance and the tuning parameters
trellis.par.set(caretTheme())
plot(knnFit1)  

# specify the metric (in this case kappa) to check other performance metrics
plot(knn.testing.cm$overall[2], metric = "Kappa")

# roc function assumes that the second class of the prdictor is the event of interest 
# so need to check this matches encoding of DV 

knn.1.roc.curve <- roc(response = testing$d14_retanined_status
                       ,predictor = knn.prob[,1]
                       ,levels = c("churned", "retained"))

# plot the ROC 
plot.roc(knn.1.roc.curve, main = 'KNN ROC')

# get model diagnostics 
auc(knn.1.roc.curve)
# get the confidence intervals for the roc 
ci(knn.1.roc.curve)


##### Varying Sample Sizes #########  

sampleSizesKNN <- function(data, start, stop, nsteps){
  # Determine interval and store sample sizes 
  interval <- (stop - start)/nsteps
  data_sizes <- seq(start, stop, by = interval)
  
  # Data frame with dummy values to store results
  results <- data.frame(sensitivity = rep(1, nsteps),
                        specificity = rep(1, nsteps),
                        size = rep(1, nsteps))
  
  # Set Controls 
  fitControl <- trainControl(method = "repeatedcv"
                             ,number = 10                        # 10 fold CV
                             ,repeats = 3                        # repeated x times
                             ,summaryFunction = twoClassSummary	# Use AUC to pick the best model
                             ,classProbs = TRUE
                             ,allowParallel = TRUE) 
  
  # Loop through nsteps KNN models: 
  for(i in 1:nsteps){
    knn.sample <-sample_n(nu.knn.d7.modeling, data_sizes[i]) 
    
    inTrain  <- createDataPartition(knn.sample$d14_retanined_status
                                    ,p = .8
                                    ,list = FALSE
                                    ,times = 1)
    
    #apply actual partictions
    training <- knn.sample[ inTrain,] 
    testing  <- knn.sample[-inTrain,]
    
    knnFit1 <- train(d14_retanined_status ~ .
                     ,data = training
                     ,method = "knn"
                     ,metric = "ROC"
                     ,trControl = fitControl,
                     preProcess = c("center", "scale"),
                     tuneLength = 5) # test k = 5 NN values
    # Test set predictions 
    testing$knn.pred <- predict(knnFit1
                                ,testing
                                ,type = "raw") # raw returns the predicted class 
    
    #Confusion Matrix  
    knn.testing.cm <- 
      confusionMatrix(data = testing$knn.pred
                      ,testing$d14_retanined_status
                      ,positive = "churned") 
    
    # Store Sensitivity and Specificity in data frame 
    results$sensitivity[i] <- knn.testing.cm$byClass[1]
    results$specificity[i] <- knn.testing.cm$byClass[2]
    results$size[i] <- data_sizes[i]
  }
  return(results)
}

## Apply function
# takes ~ 2.5 hours to go up to 120K at 10 different sample sizes 
# Robust after ~ 1K sample size 
results <- sampleSizesKNN(nu.knn.d7.modeling, 1000, 20000, 10)

# Plot results 
par(mfrow = c(1,2))
plot(results$size, results$sensitivity, main = "KNN Sensitivity by Sample Size",
     col = c("red"), xlab = "Size", ylab = "Sensitivity", pch = 16)
plot(results$size, results$specificity, main = "KNN Specificity by Sample Size",
     col = c("blue"), xlab = "Size", ylab = "Specificity", pch = 16)









