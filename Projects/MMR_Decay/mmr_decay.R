## Dominic Mullen
## 10/12/2016 

library (RPostgreSQL)
library(data.table)
library(plyr)
library(ggplot2)
library(plotly)

driver <- dbDriver("PostgreSQL") 
conn <- dbConnect(driver, host="",
                  port="",
                  dbname="",
                  user="",
                  password="")

# # Import Data 
# mmrData_sql  <- "Select 
# days_since_active
# ,pre_churn_lifetime_days
# ,avg(avg_pre_churn_mmr_diff) overall_avg_pre_churn_mmr_diff
# ,avg(avg_post_churn_mmr_diff) overall_avg_post_churn_mmr_diff        
# ,avg(final_mmr_diff) final_mmr_diff 
# From reactivated_users_4
# Group By 1,2" 
# 
# mmrData <- dbGetQuery(conn, paste("SET search_path = app139203;",mmrData_sql, sep = ""))
# 
# # data table 
# mmrData <- data.table(mmrData)
# 
# # Data exploration 
# head(mmrData)
# 
# summary(mmrData)
# dim(mmrData) # n = 8601 
# mmrData[final_mmr_diff != 0]
# # only 41 / 8607 show a change 
# 
# # Distributions 
# hist(mmrData$days_since_active)
# plot(density(mmrData$days_since_active))
# 
# hist(mmrData$pre_churn_lifetime_days)
# plot(density(mmrData$pre_churn_lifetime_days), col = "blue", lwd =  5)
# lines(density(mmrData$days_since_active), col = "red")
# # distributions of user liftimes and churn lengths look similar 
# 
# # Summary statistics 
# summary(mmrData)
# # churn liftimes and user lifetimes appear to follow the same distribution with a different mean 
# mean(mmrData$days_since_active) #50.8 
# mean(mmrData$pre_churn_lifetime_days) #43.6 
# var(mmrData$days_since_active) # 943.9
# var(mmrData$pre_churn_lifetime_days) # 977.1 
# # different means but very similar variances 
# 
# 
# # Regression Model: 
# fit1 <- lm(final_mmr_diff ~ days_since_active + pre_churn_lifetime_days, data = mmrData)
# # very slightly positive relationships between mmr and days_since_active and pre_churn_lifetime_days
# # model is likely biased towards the 37/8601 points that show a positive mmr change rather than 0. 
# # Can check this with influential points 
# plot(fit1)
# # regression model is not informative 
# 
# # MMR Bucket variable 
# mmrBucket_sql <- "select u_mmrbucket from game_match_finish where date(event_time) > current_date - 2 limit 10000"
# mmrBucket <- dbGetQuery(conn, paste("SET search_path = app139203;",mmrBucket_sql, sep = ""))
# mmrBucket$u_mmrbucket <- as.numeric(mmrBucket$u_mmrbucket)
# attach(mmrBucket)
# hist(u_mmrbucket)
# unique(u_mmrbucket)
# 
# count(u_mmrbucket >= 10) #6535 = 65%

##=======================================
## Avg 5 game pre / post elo diff 
##========================================
mmrData2_sql  <- "
Select 
 account_id
,days_since_active
,pre_churn_lifetime_days
,avg_pre_churn_elo_diff
,avg_post_churn_elo_diff
,final_elo_diff
From reactivated_accounts_d_5_match
Group By 1,2,3,4,5,6" 

mmrData2 <- dbGetQuery(conn, paste("SET search_path = app139203;",
                                   mmrData2_sql, sep = ""))

head(mmrData2)
str(mmrData2)
unique(mmrData2$final_elo_diff)

min(as.numeric(mmrData2$final_elo_diff)), max(mmrData2$final_elo_diff)

# Data exploration 
hist(mmrData2$pre_churn_lifetime_days)
hist(mmrData2$final_elo_diff, breaks = seq(from = -200, to = 200, by = 1))
count(mmrData2$final_elo_diff == 0)
count(mmrData2$final_elo_diff < 0)
# get rid of users who have 0 lifetime days before churn  


## Regression Models 
fit2 <- lm(final_mmr_diff ~ pre_churn_lifetime_days + days_since_active,
           data = mmrData2)
summary(fit2)
# both B1 and B2 are positive
# R^2 = .14 

# Suppress the intercept 
fit2.1 <- lm(final_mmr_diff ~ pre_churn_lifetime_days + days_since_active -1 ,
             data = mmrData2)
summary(fit2.1)
# both slopes are negative 
# not an improvement

# Single Explanatory Variable regression relationships 
fit3 <- lm(final_mmr_diff ~ days_since_active,
           data = mmrData2)
summary(fit3) 
# still shows a postive relationship between mmr difference and days_since_active 
fit4 <- lm(final_mmr_diff ~ pre_churn_lifetime_days,
           data = mmrData2)
summary(fit4) 


## Summary of 5 game average mmr change
count(mmrData2$final_mmr_diff == 0) 
88270/dim(mmrData2)[1] 
#32% of users show no change through 5 games
count(mmrData2$final_mmr_diff > 0) 
42624/dim(mmrData2)[1] 
#16% show an improvement 
count(mmrData2$final_mmr_diff < 0) 
132483/dim(mmrData2)[1]  
#48% of users show a decline of at least 1 mmr bucket


## histogram
# determine # of bins 
count(unique(mmrData2$final_mmr_diff)) #55 unique values
# necessary for geom_histogram() to use discrete X values  
g1 <- ggplot(mmrData2,
             aes(x = final_mmr_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle("Pre and post churn 5 game average MMR Difference")
# plotly 
plot1 <-ggplotly(g1)
plot1
# to do: Y-axis as a percentage 
  

# data table 
mmrData2 <- data.table(mmrData2)

## Create pre-churn lifetime in months variable  
mmrData2[pre_churn_lifetime_days <= 30, pre_churn_months := 1]
mmrData2[pre_churn_lifetime_days > 30 & pre_churn_lifetime_days <= 60, pre_churn_months := 2]
mmrData2[pre_churn_lifetime_days > 60 & pre_churn_lifetime_days <= 90, pre_churn_months := 3]
mmrData2[pre_churn_lifetime_days > 90 & pre_churn_lifetime_days <= 120, pre_churn_months := 4]
mmrData2[pre_churn_lifetime_days > 120 & pre_churn_lifetime_days <= 150, pre_churn_months := 5]

## Examine density plots of mmr difference by pre-churn lifetime length  
# best to compare no more than two or three at a time
plot(density(mmrData2[pre_churn_months == 1]$final_mmr_diff, na.rm = T))
lines(density(mmrData2[pre_churn_months == 2]$final_mmr_diff, na.rm = T), col = "green")
lines(density(mmrData2[pre_churn_months == 3]$final_mmr_diff, na.rm = T), col = "red")
lines(density(mmrData2[pre_churn_months == 4]$final_mmr_diff, na.rm = T), col = "blue")
lines(density(mmrData2[pre_churn_months == 5]$final_mmr_diff, na.rm = T), col = "orange")

# Summary statistics 
summary(mmrData2[pre_churn_months == 1]$final_mmr_diff)
summary(mmrData2[pre_churn_months == 2]$final_mmr_diff)
summary(mmrData2[pre_churn_months == 3]$final_mmr_diff)
summary(mmrData2[pre_churn_months == 4]$final_mmr_diff)
summary(mmrData2[pre_churn_months == 5]$final_mmr_diff)
# mean mmr difference approaches 0 as user lifetimes before churn increase 

## Create Churn length by months variable
mmrData2[days_since_active <= 30, months_since_active := 1]
mmrData2[days_since_active > 30 & days_since_active <= 60, months_since_active := 2]
mmrData2[days_since_active > 60 & days_since_active <= 90, months_since_active := 3]
mmrData2[days_since_active > 90 & days_since_active <= 120, months_since_active := 4]
mmrData2[days_since_active > 120 & days_since_active <= 150, months_since_active := 5]

## Examine density plots of mmr difference by months inactive 
# best to compare no more than two or three at a time
plot(density(mmrData2[months_since_active == 1]$final_mmr_diff, na.rm = T))
lines(density(mmrData2[months_since_active == 2]$final_mmr_diff, na.rm = T), col = "green")
lines(density(mmrData2[months_since_active == 3]$final_mmr_diff, na.rm = T), col = "red")
lines(density(mmrData2[months_since_active == 4]$final_mmr_diff, na.rm = T), col = "blue")
lines(density(mmrData2[months_since_active == 5]$final_mmr_diff, na.rm = T), col = "orange")
# each month churn period is less dense in the negative and positive regions than months = 1, 
# they are all more dense than months = 1 at 0.  This must be the reason for the positive linear 
# trend in the regression line 

# Summary statistics 
summary(mmrData2[months_since_active == 1]$final_mmr_diff)
summary(mmrData2[months_since_active == 2]$final_mmr_diff)
summary(mmrData2[months_since_active == 3]$final_mmr_diff)
summary(mmrData2[months_since_active == 4]$final_mmr_diff)
summary(mmrData2[months_since_active == 5]$final_mmr_diff) 
# Check subset sizes: 
for(i in 1:5){
  print(dim(mmrData2[months_since_active == i])[1])
}

## Scatterplots between explantory variables and mmr decay 
# relationship between user lifetime and mmr decay 
plot3 <- plot(mmrData2$pre_churn_lifetime_days[1:3000],
              mmrData2$final_mmr_diff[1:3000],
              xlab = "Pre-churn liftime (days)",
              ylab = "MMR Difference",
              main = "5 game average mmr difference pre and post
              churn by pre churn lifetime")
# at a first glance it appears the relationship is independent - no apparent linear trend 
# Examine by weeks, months, and quartiles

## relationship between churn period  and mmr decay 
plot4 <- plot(mmrData2$days_since_active[1:3000],
              mmrData2$final_mmr_diff[1:3000],
              xlab = "Days since active", 
              ylab = "MMR Difference",
              main = "5 game average mmr difference pre and post
              churn by churn period")
# seems like influential points drive the regression model towards a positive slope 


##==========================================================================================
## Data for mmr difference between last match before churn and first match upon reactivation
##==========================================================================================
mmrData3_sql  <- "Select 
 account_id
 days_since_active
,pre_churn_lifetime_days
,avg_pre_churn_elo_diff  
,avg_post_churn_elo_diff  
,final_elo_diff  
From reactivated_accounts_d_1_match" 

mmrData3 <- dbGetQuery(conn, paste("SET search_path = app139203;",mmrData3_sql, sep = ""))

unique(mmrData3$final_elo_diff) 

# Data exploration 
hist(mmrData3$final_elo_diff, breaks = 50)
dim(mmrData3)

## Histogram 
# determine # of bins: 
count(unique(mmrData3$final_mmr_diff))
# 59 unique values 
g2 <- ggplot(mmrData3,
             aes(x = final_mmr_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a legend gradient based on relative counts
                 bins = 59) + 
  ggtitle("Pre and post churn single game MMR Difference") 
plot2 <- ggplotly(g2)
plot2
