
library(tidyverse)

lifetime <- 25
rate <- 0.029/12
initial_deposit <- 500
simple_interest <- initial_deposit
compound_interest <- initial_deposit


for(i in 1:lifetime) {
  simple_interest[i+1] <- simple_interest[i] + simple_interest * rate
  compound_interest[i+1] <- compound_interest[i] * rate + compound_interest[i]
}


bind_cols(as.data.frame(seq(1, 25, 1)) %>% rename(age = 1),
          as.data.frame(simple_interest) %>% rename(simple = 1), 
          as.data.frame(compound_interest) %>% rename(compound = 1)) %>%  
  pivot_longer(., 
               simple:compound,
               names_to = "interest_rate",
               values_to = "cash_yo") %>% 
  ggplot() + 
  aes(x = age, y = cash_yo, color = interest_rate) +
  geom_line()

interest_tbl <- bind_cols(as.data.frame(seq(1, lifetime, 1)) %>% rename(age = 1), 
                          as.data.frame(rep(0.029/12, lifetime)) %>% rename(rate = 1), 
                          as.data.frame(rep(0, lifetime)) %>% rename(simple_interest = 1),
                          as.data.frame(rep(0, lifetime)) %>% rename(compound_interest = 1)) %>%  
                mutate(simple_interest = case_when(age == 1 ~ initial_deposit,
                                                   TRUE     ~ simple_interest),
                       compound_interest = case_when(age == 1 ~ initial_deposit,
                                                   TRUE     ~ compound_interest)) 


for(i in 1:lifetime) {
  interest_tbl <- interest_tbl %>% 
                  mutate(simple = case_when(age == 1 ~ simple_interest,
                                                     age > 1  ~ lag(simple_interest) + (initial_deposit * rate)),
                         compound = case_when(age == 1 ~ compound_interest,
                                                     age > 1  ~ lag(simple_interest) * rate))
}

interest_tbl %>% 
  pivot_longer(., 
               simple:compound,
               names_to = "interest_rate",
               values_to = "cash_yo") %>% 
ggplot() + 
  aes(x = age, y = cash_yo, color = interest_rate) +
  geom_line()







library(tidyverse)

growth_rate <- 0.05 # percent
growth_rate_08_perc <- 0.08 # percent
growth_rate_10_perc <- 0.1 # percent
growth_rate_20_perc <- 0.2 # percent
growth_rate_40_perc <- 0.4 # percent

df <- tibble(year = seq(2000, 2005, by = 1),
             population = seq(1, 1, length = 6))

accumulate(df$population, ~ .x * (1 + growth_rate))
accumulate(df$population, ~ .x * (1 + growth_rate_10_perc))



df %>% mutate(Interst = accumulate(population, ~ .x * (1 + growth_rate)),
              Interst_2 = first(population)*((1+growth_rate)^(row_number()-1)),
              Interst_3 = 1.05^(row_number()-1), 
              Interst_4 = cumprod(population), 
              Interest_08_perc = accumulate(population, ~ .x * (1 + growth_rate_08_perc)),
              Interest_10_perc = accumulate(population, ~ .x * (1 + growth_rate_10_perc)),
              Aave_USDT_20_perc = accumulate(population, ~ .x * (1 + growth_rate_20_perc)),
              Fulcrum_ETH_40_perc = accumulate(population, ~ .x * (1 + growth_rate_40_perc)),
              CONT_Interest_08_perc_1 = accumulate(population, ~ .x + 1 * (1 + growth_rate_08_perc)),
              CONT_Interest_08_perc_2 = accumulate(population, ~ .x * (1 + growth_rate_08_perc) + 1)
              ) %>%
              print(n = 10, width = Inf)

df %>% mutate(Calculated = first(population)*((1+growth_rate)^(row_number()-1)),
        Calculated_purr = accumulate(population, ~ .x * (1 + growth_rate)))



rerun(5, rnorm(100)) %>%
  set_names(paste0("sim", 1:5)) %>%
  map(~ accumulate(., ~ .05 + .x + .y)) %>%
  map_dfr(~ tibble(value = .x, step = 1:100), .id = "simulation") %>% # print(n =Inf, width = Inf)
  ggplot(aes(x = step, y = value)) +
    geom_line(aes(color = simulation)) +
    ggtitle("Simulations of a random walk with drift")


# annual rate of change (growth rate) with dplyr lag
data.frame(date=seq(from = as.Date("2012-08-01"),  to = as.Date("2012-10-11"), by = 'day'), 
                 GDP = cumsum(sample(c(-0.5, 3), 72, TRUE))) %>% 
  mutate(change = (GDP - lag(GDP,4)) / lag(GDP,4) * 100) %>%  #calculating the annual percentage change
  ggplot(aes(x = date, y = change)) + 
  geom_line(aes(color = change)) +
  ggtitle("Simulations of a random walk with drift") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





# Lubridate for durations 
# https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/

# seq(from = as.Date("2012-08-01"),  to = as.Date("2012-10-11"), by = 'day')

# create two date vectors 
start <- c("2012-08-21", "2012-09-01", "2012-08-15", "2012-09-18")
end <- c("2012-09-16", "2012-09-06", "2012-08-22", "2012-10-11")

# create an Interval using the %--% operator
elapsed.time <- start %--% end

as.duration(elapsed.time)

as.duration(elapsed.time) / dhours(1)

as.duration(elapsed.time) / ddays(1)

as.duration(elapsed.time) / dminutes(1)

as.duration(elapsed.time) / dyears(1)


