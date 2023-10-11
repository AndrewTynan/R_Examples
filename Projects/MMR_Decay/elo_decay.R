
library(RPostgreSQL)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)

driver <- dbDriver("PostgreSQL") 
conn <- dbConnect(driver, host="",
                  port="",
                  dbname="",
                  user="",
                  password="")

user_elo_decay_sql  <-
"Select
 account_id 
,days_since_active 
,pre_churn_lifetime_days 
,pre_post_5_match_elo_diff as final_elo_diff
From reactivated_accounts_e
Group By 1,2,3,4"

user_elo_decay <- dbGetQuery(conn, paste("SET search_path = app139203;",
                                   user_elo_decay_sql, sep = ""))

head(user_elo_decay)
str(user_elo_decay)  #389,162 obs. of  4 variables:
unique(user_elo_decay$final_elo_diff)
class(user_elo_decay$final_elo_diff)

#remove the NAs 
user_elo_decay <- filter(user_elo_decay, !is.na(final_elo_diff))
#compare obs # to previous to see how many rows were removed 
str(user_elo_decay)  #385349 obs. of  4 variables:

#get the range, using min and max, then added these into the hist below 
#-515                                            397
min(as.numeric(user_elo_decay$final_elo_diff)); max(user_elo_decay$final_elo_diff)

# dist has more negative values, but a very slightly positive avg 
# 0.24703
hist(user_elo_decay$final_elo_diff) #, col = ifelse(user_elo_decay$final_elo_diff > 0, "green", "red"))
mean(user_elo_decay$final_elo_diff) 

#not sure why the col parameter does not work when I specify breaks ?? 
#colors = ifelse(user_elo_decay$final_elo_diff > 0, "green", "red")

par(mfrow = c(1,2))
hist(user_elo_decay$final_elo_diff, 
     breaks = seq(from = (round(min(as.numeric(user_elo_decay$final_elo_diff))) -1),
                  to = (round(max(user_elo_decay$final_elo_diff)) + 1),
                  by = 1),
     main = "Avg. ELO Differece Pre/Post Reactivation",
     xlab = "Avg. ELO Differece")

#remove the outliers 
user_elo_decay_2 <- filter(user_elo_decay,
                           final_elo_diff >= -50,
                           final_elo_diff <= 50
                           )
#
str(user_elo_decay_2) #384,486 obs. of  4 variables
#trimming to range of -50 to +50 keeps 99.8% of the records 
384486 / 385349

#range and value checks 
min(as.numeric(user_elo_decay_2$final_elo_diff)); max(user_elo_decay_2$final_elo_diff)
sort(unique(round(user_elo_decay_2$final_elo_diff)))

#histograms of the trimmed datat set 
hist(user_elo_decay_2$final_elo_diff)
# final_elo_diff
hist(user_elo_decay_2$final_elo_diff, 
     breaks = seq(from = -50, to = 50, by = 1),
     main = "Outlier Removed Avg. ELO Differece Pre/Post Reactivation",
     xlab = "Avg. ELO Differece")     
     #, col = ifelse(user_elo_decay$final_elo_diff > 0, "green", "red") 

# pre_churn_lifetime_days
hist(user_elo_decay_2$pre_churn_lifetime_days, 
     breaks = seq(from = min(user_elo_decay_2$pre_churn_lifetime_days),
                  to = max(user_elo_decay_2$pre_churn_lifetime_days),
                  by = 1),
     main = "Pre-Churn Lifetime Days")

# days_since_active
hist(user_elo_decay_2$days_since_active, 
     breaks = seq(from = min(user_elo_decay_2$days_since_active),
                   to = max(user_elo_decay_2$days_since_active),
                   by = 1),
     main = "Days Since Active")

#create dsa_quartiles for days_since_active
user_elo_decay_2_a <- 
  user_elo_decay_2 %>%  
  mutate(dsa_quartiles = ntile(days_since_active, 4))

#checks
head(user_elo_decay_2_a)
str(user_elo_decay_2_a) # 384486 obs. of  5 variables:
    
#check quartile ranges 
user_elo_decay_2_a %>%  
  group_by(dsa_quartiles) %>% 
  select(days_since_active) %>% 
  summarise(min_days_since_active = min(days_since_active),
            max_days_since_active = max(days_since_active))

#plot elo diff distributions by days since active dsa_quartiles 
g1 <- ggplot(user_elo_decay_2_a,
             aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle("Histogram of Avg. ELO Diff. by Days Since Active Quartile") + 
  facet_wrap(~ dsa_quartiles, ncol = 2)

#filter days since active <= 27 (for better charting)
  #facet wrap dist. at each level of days since active 
g1b <- ggplot(subset(user_elo_decay_2_a, days_since_active <= 27),
             aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle("Histogram of Avg. ELO Diff. by Days Since Active Quartile") + 
  ggtitle(expression(atop("Avg. ELO Diff. by Days Since Active", 
                          atop(italic("Note: Showing Days Since Actve from 8 to 27"), "")))) +
  facet_wrap(~ days_since_active, 
             # scales = "free",
             ncol = 5)

#filter days since active <= 25 (for better charting)
  #facet wrap dist. at each level of days since active 
g1c <- ggplot(subset(user_elo_decay_2_a, pre_churn_lifetime_days <= 24),
              aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle(expression(atop("Histogram of Avg. ELO Diff. by Pre Churn Lifetime Days", 
                          atop(italic("Note Showing Pre Churn Lifetime Days from 0 to 24"), "")))) +
  facet_wrap(~ pre_churn_lifetime_days, 
             # scales = "free",
             ncol = 5)

# GET AVG ELO Diff. by days since active 
avg_elo_decay_by_days_since_active <-
  user_elo_decay_2_a %>%  
  select(days_since_active, final_elo_diff) %>% 
  group_by(days_since_active) %>% 
  summarise(avg_final_elo_diff = mean(final_elo_diff, na.rm = TRUE))

#plot AVG ELO Diff. by days since active 
g9 <- ggplot(avg_elo_decay_by_days_since_active,
              aes(x = days_since_active,
                  y = avg_final_elo_diff))

g9 + geom_bar(stat = "identity") +
  ggtitle("Avg. ELO Diff. by Days Since Active") +
  labs(x = "Days Since Active", y = "Avg. Final ELO Diff.") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))


# GET AVG ELO Diff. by pre_churn_lifetime_days
avg_elo_decay_by_pre_churn_lifetime_days <-
  user_elo_decay_2_a %>%  
  select(pre_churn_lifetime_days, final_elo_diff) %>% 
  group_by(pre_churn_lifetime_days) %>% 
  summarise(avg_final_elo_diff = mean(final_elo_diff, na.rm = TRUE))

#plot AVG ELO Diff. by pre_churn_lifetime_days
g10 <- ggplot(avg_elo_decay_by_pre_churn_lifetime_days,
             aes(x = pre_churn_lifetime_days,
                 y = avg_final_elo_diff
             ))
g10 + geom_bar(stat = "identity") +
  ggtitle("Avg. ELO Diff. by Pre Churn Lifetime Days") +
  labs(x = "Pre Churn Lifetime Days", y = "Avg. Final ELO Diff.") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))  

#check does show that the final_elo_diff is positive 
#0.2598319
mean(user_elo_decay_2_a$final_elo_diff)
#even the overall, pre trimmed data set is slightly positive
#0.24703
mean(user_elo_decay$final_elo_diff, na.rm = TRUE)


# Summary statistics 
user_elo_decay_2_a %>% filter(dsa_quartiles == 1) %>% summary(final_mmr_diff)
user_elo_decay_2_a %>% filter(dsa_quartiles == 2) %>% summary(final_mmr_diff)
user_elo_decay_2_a %>% filter(dsa_quartiles == 3) %>% summary(final_mmr_diff)
user_elo_decay_2_a %>% filter(dsa_quartiles == 4) %>% summary(final_mmr_diff)

# removing the users with 1 days since active and 1 pre churn lifetime days
user_elo_decay_2_b <- 
  user_elo_decay_2_a %>%  
  filter(pre_churn_lifetime_days > 1,
         days_since_active > 7) # 7 is the min, since churn is defined at 7 days

hist(user_elo_decay_2_b$days_since_active, 
     breaks = seq(from = min(user_elo_decay_2_b$days_since_active),
                   to = max(user_elo_decay_2_b$days_since_active),
                   by = 1),
     main = "Cleaned Days Since Active",
     xlab = "Days Since Active")   

hist(user_elo_decay_2_b$pre_churn_lifetime_days, 
     breaks = seq(from = min(user_elo_decay_2_b$pre_churn_lifetime_days),
                   to = max(user_elo_decay_2_b$pre_churn_lifetime_days),
                   by = 1),
     main = "Pre Churn Lifetime Days",
     xlab = "Days Since Active")

#check that 2 is the min pre_churn_lifetime_days, it is 
min(user_elo_decay_2_b$pre_churn_lifetime_days)
#check that 8 is the min days_since_active, it is 
min(user_elo_decay_2_b$days_since_active)

#check bin count 
length(unique(round(user_elo_decay_2_b$final_elo_diff)))

#plot elo diff distributions by days since active dsa_quartiles 
g9 <- ggplot(user_elo_decay_2_b,
             aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  facet_wrap(~ dsa_quartiles) + 
  ggtitle(expression(atop("Avg. ELO Diff. by Days Since Active Quartile", 
                          atop(italic("For Users with 2 or More Prechurn Lifetime Days"), "")))) 

#plot elo diff by days since active and pre churn lifetime days
# g9 <- ggplot(user_elo_decay_2_b, 
#               aes(x = round(days_since_active),
#                   y = round(pre_churn_lifetime_days),
#                   color = round(final_elo_diff)
#               ))
# #create chart 
# g9 + geom_point(alpha = 0.05) + 
#   geom_smooth() +   
#   facet_grid(. ~ dsa_quartiles,
#              switch = 'x') +  
#   ggtitle("Heros Owned By Minutes Played") + 
#   labs(x = "Heros Owned Count", y = "Minutes Played Count") +
#   theme(legend.position="bottom") + 
#   theme(axis.text.x = element_text(size = 10, angle = 00)) 

#checks
head(user_elo_decay_2_a)
str(user_elo_decay_2_a) # 384486 obs. of  5 variables:

# Single Explanatory Variable regression relationships 
fit1 <- lm(final_elo_diff ~ days_since_active,
           data = user_elo_decay_2_a)

# assess the model fit 
summary(fit1) 
confint(fit1, level=0.95) # CIs for model parameters, compare to the Estimate in Coefficients output 
anova(fit1) # anova table 
vcov(fit1) # covariance matrix for model parameters 

#scatterplot and the regression line
plot(x = user_elo_decay_2_a$days_since_active, 
     y = user_elo_decay_2_a$final_elo_diff, 
     xlim=c(min(user_elo_decay_2_a$days_since_active)-5, max(user_elo_decay_2_a$days_since_active)+5), 
     ylim=c(min(user_elo_decay_2_a$final_elo_diff)-10, max(user_elo_decay_2_a$final_elo_diff)+10))
    abline(fit1, lwd=2)

#
fit2 <- lm(final_elo_diff ~ pre_churn_lifetime_days,
           data = user_elo_decay_2_a)
summary(fit2) 
  
## how many stay the same, improve or get worse
  ##using the trimmed (between -50, +50)
#0.04405362  # 4% of reactivated users have no change (users can reactivate multiple times)
user_elo_decay_2_a %>% filter(final_elo_diff == 0) %>% summarise(count = n()) / dim(user_elo_decay_2_a)[1] 
#0.4884469  # 49% of reactivated users show an improvement 
user_elo_decay_2_a %>% filter(final_elo_diff > 0) %>% summarise(count = n()) / dim(user_elo_decay_2_a)[1] 
#0.4674995  # 47% of reactivated users show a decline 
user_elo_decay_2_a %>% filter(final_elo_diff < 0) %>% summarise(count = n()) / dim(user_elo_decay_2_a)[1] 

## how many stay the same, improve or get worse
  ##using the trimmed (between -50, +50)
#0.04405362  # 4% of reactivated users have no change (users can reactivate multiple times)
user_elo_decay_2_a %>% filter(final_elo_diff == 0) %>% summarise(count = n()) / dim(user_elo_decay_2_a)[1] 
#0.4884469  # 49% of reactivated users show an improvement 
user_elo_decay_2_a %>% filter(final_elo_diff > 0) %>% summarise(count = n()) / dim(user_elo_decay_2_a)[1] 
#0.4674995  # 47% of reactivated users show a decline 
user_elo_decay_2_a %>% filter(final_elo_diff < 0) %>% summarise(count = n()) / dim(user_elo_decay_2_a)[1] 

## how many stay the same, improve or get worse
  ##using the trimmed (between -50, +50)
  ## and removed all records w/ pre_churn_lifetime_days > 1 and days_since_active > 7
#0.02891323  # 4% of reactivated users have no change (users can reactivate multiple times)
user_elo_decay_2_b %>% filter(final_elo_diff == 0) %>% summarise(count = n()) / dim(user_elo_decay_2_b)[1] 
#0.4969819  # 49% of reactivated users show an improvement 
user_elo_decay_2_b %>% filter(final_elo_diff > 0) %>% summarise(count = n()) / dim(user_elo_decay_2_b)[1] 
#0.4741049  # 47% of reactivated users show a decline 
user_elo_decay_2_b %>% filter(final_elo_diff < 0) %>% summarise(count = n()) / dim(user_elo_decay_2_b)[1] 

#the means of final_elo_diff in all stages of cleaning the data is positive 
#0.24703
mean(user_elo_decay$final_elo_diff, na.rm = TRUE)
#0.2598319
mean(user_elo_decay_2$final_elo_diff) #this is the same as user_elo_decay_2_a (which added another field, but did not chagne final_elo_diff)
#0.2648733
mean(user_elo_decay_2_b$final_elo_diff)


#round fianl elo diff & get percentages at each bucket 
  #data is from -50 to 50 range of final_elo_diff 
final_elo_diff_freq <-
user_elo_decay_2_a %>%  
  select(account_id, final_elo_diff) %>% 
  transmute(account_id, final_elo_diff = as.integer(round(final_elo_diff,0))) %>% 
  group_by(final_elo_diff) %>% 
  summarise (n = n()) %>%
  mutate(perc = n / sum(n)) 

final_elo_diff_freq <- as.data.frame(final_elo_diff_freq)





