### LDA / QDA Model Predictions and Performance ### 

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
nu.lda.d7.modeling <- nu_clustering_d7[, c('amplitude_id'
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
nu.lda.d7.modeling.samp.10k <-sample_n(nu.lda.d7.modeling, 10000) 

# create separate version of the modeling data for use with KNN 
forLDA <- nu.lda.d7.modeling.samp.10k

# partition the data for training and testing
set.seed(1234)
inTrain  <- createDataPartition(forLDA$d14_retanined_status
                                ,p = .8
                                ,list = FALSE
                                ,times = 1)

#apply actual partictions
training <- nu.lda.d7.modeling.samp.10k[ inTrain,] 
testing <- nu.lda.d7.modeling.samp.10k[-inTrain,]

nrow(training)
nrow(testing)

# set resampling parameters
fitControl <- trainControl(method = "repeatedcv"
                           ,number = 10                        # 10 fold CV
                           ,repeats = 3                        # repeated x times
                           ,summaryFunction = twoClassSummary	# Use AUC to pick the best model
                           ,classProbs = TRUE
                           ,allowParallel = TRUE) 

set.seed(1234)
ldaFit1 <- train(d14_retanined_status ~ . - amplitude_id 
                 ,data = training
                 ,method = "lda"
                 ,metric = "ROC"
                 ,trControl = fitControl)

# Alternative parameters 
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
ldaFit2 <- train(d14_retanined_status ~ . - amplitude_id,
                 data = training,
                 method = "lda",
                 trControl = ctrl,
                 preProcess = c("center","scale"))

# Make predictions using the test data set
# get predicted classes 
testing$lda.pred <- predict(ldaFit1
                            ,testing
                            ,type = "raw") # raw returns the predicted class 

# get predicted class probabilities  
lda.prob <- predict(ldaFit1
                    ,testing
                    ,type = "prob")   

#Look at the confusion matrix  
lda.testing.cm <- 
  confusionMatrix(data = testing$lda.pred
                  ,testing$d14_retanined_status
                  ,positive = "churned") 

# roc function assumes that the second class of the prdictor is the event of interest 
# so need to check this matches encoding of DV 

lda.1.roc.curve <- roc(response = testing$d14_retanined_status
                       ,predictor = lda.prob[,1]
                       ,levels = c("churned", "retained"))

# plot the ROC 
plot.roc(lda.1.roc.curve, main = 'LDA ROC')

# get model diagnostics 
auc(lda.1.roc.curve)
# get the confidence intervals for the roc 
ci(lda.1.roc.curve)


##### Varying Sample Sizes #########  

sampleSizesLDA <- function(data, start, stop, nsteps){
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
    lda.sample <-sample_n(nu.lda.d7.modeling, data_sizes[i]) 
    
    inTrain  <- createDataPartition(lda.sample$d14_retanined_status
                                    ,p = .8
                                    ,list = FALSE
                                    ,times = 1)
    
    #apply actual partictions
    training <- lda.sample[ inTrain,] 
    testing  <- lda.sample[-inTrain,]
    
    ldaFit1 <- train(d14_retanined_status ~ . - amplitude_id
                     ,data = training
                     ,method = "lda"
                     ,metric = "ROC"
                     ,trControl = fitControl) 
    # Test set predictions 
    testing$lda.pred <- predict(ldaFit1
                                ,testing
                                ,type = "raw") # raw returns the predicted class 
    
    #Confusion Matrix  
    lda.testing.cm <- 
      confusionMatrix(data = testing$lda.pred
                      ,testing$d14_retanined_status
                      ,positive = "churned") 
    
    # Store Sensitivity and Specificity in data frame 
    results$sensitivity[i] <- lda.testing.cm$byClass[1]
    results$specificity[i] <- lda.testing.cm$byClass[2]
    results$size[i] <- data_sizes[i]
  }
  return(results)
}

## Apply function 
resultsLDA <- sampleSizesLDA(nu.lda.d7.modeling, 1000, 20000, 10)

# Plot results 
par(mfrow = c(1,2))
plot(resultsLDA$size, resultsLDA$sensitivity, main = "KNN Sensitivity by Sample Size",
     col = c("red"), xlab = "Size", ylab = "Sensitivity", pch = 16)
plot(resultsLDA$size, resultsLDA$specificity, main = "KNN Specificity by Sample Size",
     col = c("blue"), xlab = "Size", ylab = "Specificity", pch = 16)


############## QDA ###############################

nu.qda.d7.modeling <- nu.lda.d7.modeling

# sample down while working 
nu.qda.d7.modeling.samp.10k <-sample_n(nu.lda.d7.modeling, 10000) 

# create separate version of the modeling data for use with KNN 
forqda <- nu.qda.d7.modeling.samp.10k

# partition the data for training and testing
set.seed(1234)
inTrain  <- createDataPartition(forqda$d14_retanined_status
                                ,p = .8
                                ,list = FALSE
                                ,times = 1)

#apply actual partictions
training <- nu.qda.d7.modeling.samp.10k[ inTrain,] 
testing <- nu.qda.d7.modeling.samp.10k[-inTrain,]

nrow(training)
nrow(testing)

# set resampling parameters
fitControl <- trainControl(method = "repeatedcv"
                           ,number = 10                        # 10 fold CV
                           ,repeats = 3                        # repeated x times
                           ,summaryFunction = twoClassSummary	# Use AUC to pick the best model
                           ,classProbs = TRUE
                           ,allowParallel = TRUE) 

set.seed(1234)
qdaFit1 <- train(d14_retanined_status ~ . - amplitude_id 
                 ,data = training
                 ,method = "qda"
                 ,metric = "ROC"
                 ,trControl = fitControl)

# Alternative parameters 
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
qdaFit2 <- train(d14_retanined_status ~ . - amplitude_id,
                 data = training,
                 method = "qda",
                 trControl = ctrl,
                 preProcess = c("center","scale"))

# Make predictions using the test data set
# get predicted classes 
testing$qda.pred <- predict(qdaFit1
                            ,testing
                            ,type = "raw") # raw returns the predicted class 

# get predicted class probabilities  
qda.prob <- predict(qdaFit1
                    ,testing
                    ,type = "prob")   

#Look at the confusion matrix  
qda.testing.cm <- 
  confusionMatrix(data = testing$qda.pred
                  ,testing$d14_retanined_status
                  ,positive = "churned") 

# roc function assumes that the second class of the prdictor is the event of interest 
# so need to check this matches encoding of DV 

qda.1.roc.curve <- roc(response = testing$d14_retanined_status
                       ,predictor = qda.prob[,1]
                       ,levels = c("churned", "retained"))

# plot the ROC 
plot.roc(qda.1.roc.curve, main = 'qda ROC')

# get model diagnostics 
auc(qda.1.roc.curve)
# get the confidence intervals for the roc 
ci(qda.1.roc.curve)


##### Varying Sample Sizes #########  

sampleSizesQDA <- function(data, start, stop, nsteps){
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
    qda.sample <-sample_n(nu.qda.d7.modeling, data_sizes[i]) 
    
    inTrain  <- createDataPartition(qda.sample$d14_retanined_status
                                    ,p = .8
                                    ,list = FALSE
                                    ,times = 1)
    
    #apply actual partictions
    training <- qda.sample[ inTrain,] 
    testing  <- qda.sample[-inTrain,]
    
    qdaFit1 <- train(d14_retanined_status ~ . - amplitude_id 
                     ,data = training
                     ,method = "qda"
                     ,metric = "ROC"
                     ,trControl = fitControl) 
    # Test set predictions 
    testing$qda.pred <- predict(qdaFit1
                                ,testing
                                ,type = "raw") # raw returns the predicted class 
    
    #Confusion Matrix  
    qda.testing.cm <- 
      confusionMatrix(data = testing$qda.pred
                      ,testing$d14_retanined_status
                      ,positive = "churned") 
    
    # Store Sensitivity and Specificity in data frame 
    results$sensitivity[i] <- qda.testing.cm$byClass[1]
    results$specificity[i] <- qda.testing.cm$byClass[2]
    results$size[i] <- data_sizes[i]
  }
  return(results)
}

## Apply function 
resultsQDA <- sampleSizesQDA(nu.qda.d7.modeling, 1000, 20000, 10)

# Plot results 
par(mfrow = c(1,2))
plot(resultsQDA$size, resultsQDA$sensitivity, main = "QDA Sensitivity by Sample Size",
     col = c("red"), xlab = "Size", ylab = "Sensitivity", pch = 16)
plot(resultsQDA$size, resultsQDA$specificity, main = "QDA Specificity by Sample Size",
     col = c("blue"), xlab = "Size", ylab = "Specificity", pch = 16)


















