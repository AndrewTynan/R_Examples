

library(RPostgreSQL)
library(plyr)
library(dplyr)
library(ggplot2)

driver <- dbDriver("PostgreSQL") 
conn <- dbConnect(driver, host="",
                  port="",
                  dbname="",
                  user="",
                  password="")

user_elo_decay_b_1_match_sql  <-
"Select
account_id 
,event_time 
,event_time_rank       
,first_login_date
,days_since_active
,pre_churn_lifetime_days 
,player_elo 
,player_elo_lag  
,pre_post_1_match_elo_diff as final_elo_diff
From reactivated_accounts_e_v2
Group By 1,2,3,4,5,6,7,8,9"

user_elo_decay_b <- dbGetQuery(conn, paste("SET search_path = app139203;",
                                         user_elo_decay_b_1_match_sql, sep = ""))

head(user_elo_decay_b)
str(user_elo_decay_b)  #389,162 obs. of  4 variables:
sort(unique(user_elo_decay_b$final_elo_diff))
class(user_elo_decay_b$final_elo_diff)

#how many are NA 
  # 155,443 this is quite large 
sum(is.na(user_elo_decay_b$final_elo_diff))

#remove the NA values 
user_elo_decay_b <- filter(user_elo_decay_b, !is.na(final_elo_diff))

str(user_elo_decay_b)  #233,719 obs. of  4 variables:

hist(user_elo_decay_b$final_elo_diff)

mean(user_elo_decay_b$final_elo_diff)

par(mfrow = c(1,2))

hist(user_elo_decay_b$final_elo_diff,
     breaks = seq(from = min(user_elo_decay_b$final_elo_diff),
                  to = max(user_elo_decay_b$final_elo_diff),
                  by = 1),
     main = "ELO Differece Pre/Post Reactivation",
     xlab = "ELO Differece")

user_elo_decay_b_2 <- filter(user_elo_decay_b,
                           final_elo_diff >= -100,
                           final_elo_diff <= 100)

str(user_elo_decay_b_2) #230,930 obs. of  4 variables
# keeps 0.9880669 of the records, but removing the NAs one step above removed a large # or rows 
230930 / 233719 

#range checks 
min(as.numeric(user_elo_decay_b_2$final_elo_diff)); max(user_elo_decay_b_2$final_elo_diff)

# final_elo_diff
hist(user_elo_decay_b_2$final_elo_diff, 
     breaks = seq(from = -100, to = 100, by = 1),
     main = "Outlier Removed \n ELO Differece Pre/Post Reactivation",
     xlab = "ELO Differece")   


#create dsa_quartiles for days_since_active
user_elo_decay_b_2_a <- 
  user_elo_decay_b_2 %>%  
  mutate(dsa_quartiles = ntile(days_since_active, 4))

#checks
head(user_elo_decay_b_2_a)
str(user_elo_decay_b_2_a) 

#plot elo diff distributions by days since active dsa_quartiles 
gg1 <- ggplot(user_elo_decay_b_2_a,
             aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle("Histogram of ELO Diff. by Days Since Active Quartile") + 
  facet_wrap(~ dsa_quartiles, ncol = 2)

#filter days since active <= 27 (for better charting)
#facet wrap dist. at each level of days since active 
gg1b <- ggplot(subset(user_elo_decay_b_2_a, days_since_active <= 27),
              aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle("Histogram of ELO Diff. by Days Since Active Quartile") + 
  ggtitle(expression(atop("ELO Diff. by Days Since Active                ", 
                          atop(italic("Note Showing Days Since Actve from 8 to 27"), "")))) +
  facet_wrap(~ days_since_active, 
             # scales = "free",
             ncol = 5)

#filter days since active <= 25 (for better charting)
#facet wrap dist. at each level of days since active 
gg1c <- ggplot(subset(user_elo_decay_b_2_a, pre_churn_lifetime_days <= 24),
              aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle(expression(atop("Histogram of Avg. ELO Diff. by Pre Churn Lifetime Days", 
                          atop(italic("Note Showing Pre Churn Lifetime Days from 0 to 24"), "")))) +
  facet_wrap(~ pre_churn_lifetime_days, 
             # scales = "free",
             ncol = 5)

# GET AVG ELO Diff. by days since active 
  #naming it 2 so that there is no conflict between scripts
avg_elo_decay_by_days_since_active_2 <-
  user_elo_decay_b_2_a %>%  
  select(days_since_active, final_elo_diff) %>% 
  group_by(days_since_active) %>% 
  summarise(avg_final_elo_diff = mean(final_elo_diff, na.rm = TRUE))

head(avg_elo_decay_by_days_since_active_2)

#plot AVG ELO Diff. by days since active 
gg9 <- ggplot(avg_elo_decay_by_days_since_active_2,
             aes(x = days_since_active,
                 y = avg_final_elo_diff)) + 
  geom_bar(stat = "identity") +
  ggtitle("Avg. ELO Diff. by Days Since Active") +
  labs(x = "Days Since Active", y = "Avg. Final ELO Diff.") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))


# GET AVG ELO Diff. by pre_churn_lifetime_days
  #naming it 2 so that there is no conflict between scripts
avg_elo_decay_by_pre_churn_lifetime_days_2 <-
  user_elo_decay_b_2_a %>%  
  select(pre_churn_lifetime_days, final_elo_diff) %>% 
  group_by(pre_churn_lifetime_days) %>% 
  summarise(avg_final_elo_diff = mean(final_elo_diff, na.rm = TRUE))

#plot AVG ELO Diff. by pre_churn_lifetime_days
gg10 <- ggplot(avg_elo_decay_by_pre_churn_lifetime_days_2,
              aes(x = pre_churn_lifetime_days,
                  y = avg_final_elo_diff
              )) + 
  geom_bar(stat = "identity") +
  ggtitle("Avg. ELO Diff. by Pre Churn Lifetime Days") +
  labs(x = "Pre Churn Lifetime Days", y = "Avg. Final ELO Diff.") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))  


#create player_elo buckets 
user_elo_decay_b_2_a <-
  user_elo_decay_b_2_a %>%
  transform(player_elo_buckets = floor(player_elo / 100))

#check player_elo_buckets ranges 
user_elo_decay_b_2_a_buckets_check <- 
  user_elo_decay_b_2_a %>%
  filter(player_elo, player_elo_buckets) %>% 
  group_by(player_elo_buckets) %>% 
  summarise(min_player_elo = min(player_elo),
            max_player_elo = max(player_elo))

#annoying way to print entire tibble 
print(tbl_df(user_elo_decay_b_2_a_buckets_check), n=40)

#plot elo diff distributions by player_elo_buckets
gg11 <- ggplot(user_elo_decay_b_2_a,
             aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle("Histogram of ELO Diff. by ELO Bucket") + 
  facet_wrap(~ player_elo_buckets, ncol = 5)


head(user_elo_decay_b_2_a)

# GET AVG ELO Diff. by player_elo_buckets
  # adding 2 at end to avoid conflicting names between scripts
avg_elo_decay_by_player_elo_buckets_2 <-
  user_elo_decay_b_2_a %>%  
  select(player_elo_buckets, final_elo_diff) %>% 
  group_by(player_elo_buckets) %>% 
  summarise(avg_final_elo_diff = mean(final_elo_diff, na.rm = TRUE))

#check data 
print(tbl_df(avg_elo_decay_by_player_elo_buckets_2), n=30)

#plot AVG ELO Diff. by player_elo_buckets
gg12 <- ggplot(avg_elo_decay_by_player_elo_buckets_2,
              aes(x = player_elo_buckets,
                  y = avg_final_elo_diff)) + 
  geom_bar(stat = "identity") +
  ggtitle("Avg. ELO Diff. by ELO Bucket") +
  labs(x = "ELO Bucket", y = "Avg. ELO Diff.") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))  





