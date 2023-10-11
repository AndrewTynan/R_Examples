
# Mark: high level avg of the pre and post periods (no buckets)
# 2) add chart of avg ELO Diff by ELO_Buckets 

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

# Note: this adds player_elo, which is then bucketed and used as a grouping variable 
# Doing it separately here to keep the elo_decay.R script from duplicating things / being messy 

user_elo_decay_n_sql  <-
"Select
account_id 
,player_elo
,days_since_active 
,pre_churn_lifetime_days 
,avg_pre_churn_player_elo 
,avg_post_churn_player_elo 
,(avg_post_churn_player_elo - avg_pre_churn_player_elo) as pre_post_avg_player_elo_diff
,pre_post_5_match_elo_diff as final_elo_diff
From reactivated_accounts_e
Group By 1,2,3,4,5,6,7,8"

user_elo_decay_n <- dbGetQuery(conn, paste("SET search_path = app139203;",
                                           user_elo_decay_n_sql, sep = ""))

head(user_elo_decay_n)
str(user_elo_decay_n)  #389,168 obs. of  4 variables:

#remove the NAs 
user_elo_decay_n <- filter(user_elo_decay_n, !is.na(final_elo_diff))
#compare obs # to previous to see how many rows were removed 
str(user_elo_decay_n)  #385355 obs. of  4 variables:

# dist has more negative values, but a very slightly positive avg 
# 0.2476781
mean(user_elo_decay_n$final_elo_diff) 

#create player_elo buckets 
user_elo_decay_n <-
  user_elo_decay_n %>%
  transform(player_elo_buckets = floor(player_elo / 100))

#check player_elo_buckets ranges 
player_elo_buckets_check <- 
  user_elo_decay_n %>%
  filter(player_elo, player_elo_buckets) %>% 
  group_by(player_elo_buckets) %>% 
  summarise(min_player_elo = min(player_elo),
            max_player_elo = max(player_elo))

#annoying way to print entire tibble 
print(tbl_df(player_elo_buckets_check), n=40)

#plot elo diff distributions by player_elo_buckets
g11a <- ggplot(subset(user_elo_decay_n, 
                      player_elo_buckets >= 5 & 
                        player_elo_buckets <= 24),
             aes(x = final_elo_diff)) + 
  geom_histogram(aes(fill = ..count..),  # creates a gradient legend by relative counts 
                 bins = 55) + 
  ggtitle("Histogram of Avg. ELO Diff. by ELO Bucket") + 
  facet_wrap(~ player_elo_buckets, 
             scales = "free",             
             ncol = 5) 

head(user_elo_decay_n)

# GET AVG ELO Diff. by player_elo_buckets
avg_elo_decay_by_player_elo_buckets <-
  user_elo_decay_n %>%  
  select(player_elo_buckets, final_elo_diff) %>% 
  group_by(player_elo_buckets) %>% 
  summarise(avg_final_elo_diff = mean(final_elo_diff, na.rm = TRUE))

#check data 
print(tbl_df(avg_elo_decay_by_player_elo_buckets), n=30)

#plot AVG ELO Diff. by player_elo_buckets
g12 <- ggplot(avg_elo_decay_by_player_elo_buckets,
              aes(x = player_elo_buckets,
                  y = avg_final_elo_diff))

g12 + geom_bar(stat = "identity") +
  ggtitle("Avg. ELO Diff. by ELO Bucket") +
  labs(x = "ELO Bucket", y = "Avg. ELO Diff.") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))  

# get counts of users by elo buckets (for making dist. chart)
user_count_by_player_elo_buckets <-
  user_elo_decay_n %>%  
  select(account_id, player_elo_buckets) %>% 
  group_by(player_elo_buckets) %>% 
  summarise(user_count = n()) 

print(tbl_df(user_count_by_player_elo_buckets), n = 30)

#plot AVG ELO Diff. by pre_churn_lifetime_days
g13 <- ggplot(user_count_by_player_elo_buckets,
              aes(x = player_elo_buckets,
                  y = user_count))

g13 + geom_bar(stat = "identity") +
  ggtitle("User Count by ELO Bucket") +
  labs(x = "ELO Bucket", y = "User Count") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))  

## NOW Chart the avg pre post ELOs and then bind together and get diff.
# GET AVG ELO Diff. by player_elo_buckets
overall_pre_post_avg_player_elo_diff <-
  user_elo_decay_n %>%  
  select(player_elo_buckets
        ,avg_pre_churn_player_elo
        ,avg_post_churn_player_elo
        ,pre_post_avg_player_elo_diff) %>% 
  group_by(player_elo_buckets) %>% 
  summarise(overall_avg_pre_churn_player_elo = mean(avg_pre_churn_player_elo, na.rm = TRUE),
            overall_avg_post_churn_player_elo = mean(avg_post_churn_player_elo, na.rm = TRUE),
            overall_pre_post_avg_player_elo_diff = mean(pre_post_avg_player_elo_diff, na.rm = TRUE)
            )

print(tbl_df(overall_pre_post_avg_player_elo_diff), n = 30)

#plot overall_pre_post_avg_player_elo_diff
g14 <- ggplot(overall_pre_post_avg_player_elo_diff,
              aes(x = player_elo_buckets,
                  y = overall_pre_post_avg_player_elo_diff))

g14 + geom_bar(stat = "identity") +
  ggtitle("Overall AVG ELO Diff Between Abs. Value ELO Pre/Post") +
  labs(x = "ELO Bucket", y = "Overall AVG ELO Diff Between Abs. Value ELO Pre/Post") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(size = 10, angle = 00))  


