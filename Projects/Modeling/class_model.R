
### GBM Model Predictions and Performance ### 

library(caret)
library(pROC)

# need to source the data 
source("Import_and_Explore.R")

# dev.off()

# str(nu_clustering_d7)

######### Preprocessing Data ######### 

#chceck for NAs 
sapply(nu_clustering_d7, function(y) sum(length(which(is.na(y)))))
#turnall of the NAs into 0 
nu_clustering_d7[is.na(nu_clustering_d7)] <- 0

# get modeling data 
nu.gbm.d7.modeling <- nu_clustering_d7[, c('amplitude_id'
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
# str(nu.gbm.d7.modeling)

# sample down while working 
nu.gbm.d7.modeling.samp.10k <-sample_n(nu.gbm.d7.modeling, 10000) 

# create separate version of the modeling data for use with GBM 
forGBM <- nu.gbm.d7.modeling.samp.10k

# recode the DV to be numeric 
forGBM$d14_retanined_status <- ifelse(forGBM$d14_retanined_status == "churned", 1, 0)

# partition the data for training and testing
set.seed(1234)
inTrain  <- createDataPartition(forGBM$d14_retanined_status
                                ,p = .8
                                ,list = FALSE
                                ,times = 1)

#apply actual partictions
training <- nu.gbm.d7.modeling.samp.10k[ inTrain,] 
testing <- nu.gbm.d7.modeling.samp.10k[-inTrain,]

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
gbmFit1 <- train(d14_retanined_status ~ .
                 ,data = training
                 ,method = "gbm"
                 ,metric = "ROC"
                 ,trControl = fitControl
                 #,summaryFunction = twoClassSummary    # already specified in fitControl
                 ,verbose = FALSE)

# Make predictions using the test data set
# get predicted classes 
testing$gbm.pred <- predict(gbmFit1
                            ,testing
                            ,type = "raw") # raw returns the predicted class 

str(gbmFit1)

# get predicted class probabilities  
gbm.prob <- predict(gbmFit1
                   ,testing
                   ,type = "prob")    

head(gbm.prob)
str(testing)

testing$gbm.prob <- gbm.prob[,"churned"]

head(testing)
str(testing)

#Look at the confusion matrix  
gbm.testing.cm <- 
confusionMatrix(data = testing$gbm.pred
               ,testing$d14_retanined_status
               ,positive = "churned") 

# Plotting the Resampling Profile
# to examine relationship between the estimates of performance and the tuning parameters
trellis.par.set(caretTheme())
plot(gbmFit1)  

# specify the metric (in this case kappa) to check other performance metrics
plot(gbmFit1, metric = "Kappa")

# roc function assumes that the second class of the prdictor is the event of interest 
# so need to check this matches encoding of DV 

gbm.1.roc.curve <- roc(response = testing$d14_retanined_status
                       ,predictor = gbm.prob[,1]
                       ,levels = c("churned", "retained"))

# plot the ROC 
plot.roc(gbm.1.roc.curve, main = 'GBM ROC')

# get model diagnostics 
auc(gbm.1.roc.curve)
# get the confidence intervals for the roc 
ci(gbm.1.roc.curve)


# probability calibration 
gbm.1.cal.curve <- calibration(d14_retanined_status ~ .
                              ,data = testing)

xyplot(gbm.1.cal.curve
       ,auto.key = list(columns))

######### Second GBM Model Using Grid Search ######### 

# set custom parameters for gbm
# using the sights gained from prior tree exlorations to set interaction.depth
gbmGrid <-  expand.grid(interaction.depth = c(1, 4, 7)
                        ,n.trees = (1:30)*25  #50 
                        ,shrinkage = 0.1  # 0.01
                        ,n.minobsinnode = 2000) 

nrow(gbmGrid)

# use gbmGrid specify the exact models to evaluate
set.seed(1234) 
gbmFit2 <- train(d14_retanined_status ~ .
                 ,data = training
                 ,method = "gbm" 
                 ,metric = "ROC"
                 ,trControl = fitControl
                 ,verbose = FALSE
                 ,tuneGrid = gbmGrid) 

# create a second test object 
# this is different model and needs a new test object (of the same test data)
testing.2 <- nu.gbm.d7.modeling[-inTrain,]

# Make predictions using the test data set
# get predicted classes 
testing.2$gbm.pred <- predict(gbmFit2
                             ,testing.2
                             ,type = "raw") # raw returns the predicted class 

# get predicted class probabilities  
# do not attach to testing (need to split the prob class below)
gbmFit2.prob <- predict(gbmFit2
                       ,testing.2
                       ,type = "prob")  

head(gbmFit2.prob)
str(gbmFit2.prob)

testing.2$gbm.prob <- gbmFit2.prob[,"churned"]

head(testing.2)
str(testing.2)

#Look at the confusion matrix  
confusionMatrix(data = testing.2$gbm.pred
                ,testing.2$d14_retanined_status
                ,positive = "churned") 

# Plotting the Resampling Profile
# to examine relationship between the estimates of performance and the tuning parameters
trellis.par.set(caretTheme())
plot(gbmFit2)  

# specify the metric (in this case kappa) to check other performance metrics
plot(gbmFit2, metric = "Kappa")

# roc function assumes that the second class of the prdictor is the event of interest 
# so need to check this matches encoding of DV 

gbm.1.roc.curve <- roc(response = testing$gbm.pred
                       ,predictor = testing$gbm.prob
                       ,levels = rev(levels(forGBM$d14_retanined_status))) # reversed the DV levels, here 2nd class is class of interest

# head(testing$gbm.prob[,2])

auc(gbm.1.roc.curve)

ci.roc(gbm.1.roc.curve)

plot(gbm.1.roc.curve)


# probability calibration 
gbm.1.cal.curve <- calibration(d14_retanined_status ~ .
                               ,data = training)

xyplot(gbm.1.cal.curve
       ,auto.key = list(columns))


###### Other 

# pass crtl to trControl arg 
# plsFit <- train(d14_retanined_status ~ .
#                   ,data = training
#                   ,method = "pls"
#                   ,tuneLength = 15
#                   ,trControl = ctrl
#                   ,preProc = c("center", "scale"))
# 
# 
# ctrl <- trainControl(method = "repeatedcv"
#                        ,repeats = 3
#                        ,classProbs = TRUE
#                        ,summaryFunction = twoClassSummary)
# # 
# plsFit <- train(d14_retanined_status ~ .
#                   ,data = training
#                   ,method = "pls"
#                   ,tuneLength = 15
#                   ,trControl = ctrl
#                   ,metric = "ROC"
#                   ,preProc = c("center", "scale"))










