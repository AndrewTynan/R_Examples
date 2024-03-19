
library(tidyverse)
library(broom)
library(rstatix)
library(glue)
options(scipen=999)

(.packages())

sem <- function(x, na_rm = FALSE) {
  out <- sd(x, na.rm = na_rm) / sqrt(length(x))
  return(out)
}

# simulate different retention rates 
suppressWarnings(rm(t_test_results, prop_test_results)) 

exposed_n <- 190000
control_n <- 10000

for(x in 1:1000) {

  # simulate scalar value 
  exposed_retention_rate <- rnorm(1, mean=62, sd=0.8) / 100
  control_retention_rate <- rnorm(1, mean=61, sd=0.8) / 100 

  # generate sequence 
  exposed <- rbinom(n = exposed_n, size = 1, prob = exposed_retention_rate)
  control <- rbinom(n = control_n, size = 1, prob = control_retention_rate)

  # run tests and store results 
  t_test_output <- tidy(t.test(exposed, control)) 

  prop_test_output <- tidy(prop.test(x = c(exposed_retention_rate * exposed_n, 
                                           control_retention_rate * control_n),
                                    n = c(exposed_n, control_n)))

  if(!exists("t_test_results")) { 
    t_test_results <- t_test_output    
  } else {
    t_test_results <- bind_rows(t_test_results, t_test_output)
  }

  if(!exists("prop_test_results")) { 
    prop_test_results <- prop_test_output    
  } else {
    prop_test_results <- bind_rows(prop_test_results, prop_test_output)
  }  

} 

t_test_results <- t_test_results %>%
                  rename(mean_diff   = estimate, 
                         x_mean      = estimate1,
                         y_mean      = estimate2,
                         t_statistic = statistic)

prop_test_results <- prop_test_results %>%
                     rename(x_prop    = estimate1,
                            y_prop    = estimate2,
                            X_squared = statistic,
                            df        = parameter)


# wrapped in a function 
percent_ttest_proptest_sim <- function(iter = 1000, exposed_n, control_n, baseline_rate, exposed_rate, stddev, output_df_name) {

  #' @description 
  #' `percent_ttest_proptest_sim` returns 
  #'  of the `var` arguemnt for all variable in the`by` argument for the data.frame in the `data` argument 
  #'
  #' @details 
  #' it depends on R's stats package for rnorm, rbinom, t.test, and prop.test
  #' it depends on tidyverse for rename, bind_rows, and %>%
  #' it depends on tidyr for tidy() 
  #'
  #' @iter number of iterations, default is 1,000
  #' @param exposed_n an integer sample size of the exposed segment 
  #' @param control_n an integer sample size of the control segment 
  #' @param baseline_rate the expected retention rate percent for the exposed users
  #' @param exposed_rate the expected retention rate percent for the exposed users
  #' @param stddev the standard deviaiton of baseline_rate (also used for the exposed_rate)
  #' @param output_df_name a character string to name the output data.frame()'s 

  for(x in 1:1000) {

    # simulate scalar value 
    exposed_retention_rate <- rnorm(1, mean = exposed_rate, sd = stddev) / 100
    control_retention_rate <- rnorm(1, mean = baseline_rate, sd = stddev) / 100 

    # generate sequence 
    exposed <- rbinom(n = exposed_n, size = 1, prob = exposed_retention_rate)
    control <- rbinom(n = control_n, size = 1, prob = control_retention_rate)

    # run tests and store results 
    t_test_output <- tidy(t.test(exposed, control)) 

    prop_test_output <- tidy(prop.test(x = c(exposed_retention_rate * exposed_n, 
                                             control_retention_rate * control_n),
                                      n = c(exposed_n, control_n)))

    if(!exists("t_test_results")) { 
      t_test_results <- t_test_output    
    } else {
      t_test_results <- bind_rows(t_test_results, t_test_output)
    }

    if(!exists("prop_test_results")) { 
      prop_test_results <- prop_test_output    
    } else {
      prop_test_results <- bind_rows(prop_test_results, prop_test_output)
    }  

  } 

  t_test_results <- t_test_results %>%
                    rename(mean_diff   = estimate, 
                          x_mean      = estimate1,
                          y_mean      = estimate2,
                          t_statistic = statistic)

  prop_test_results <- prop_test_results %>%
                      rename(x_prop    = estimate1,
                              y_prop    = estimate2,
                              X_squared = statistic,
                              df        = parameter)

  # output_df_name_string  <- as_label(enquo(output_df_name)) 
  output_df_name_string  <- output_df_name
  ttest_name             <- glue('t_test_results_{output_df_name_string}') 
  prop_test_name         <- glue('prop_test_results_{output_df_name_string}') 

  assign(ttest_name, t_test_results, envir = .GlobalEnv) 
  assign(prop_test_name, prop_test_results, envir = .GlobalEnv)   
  print(glue('Created new data.frame() called: 
              {ttest_name}
              {prop_test_name}'
              ))   
  # suppressWarnings(rm(exposed_retention_rate, control_retention_rate, exposed, control, t_test_output, prop_test_output, t_test_results, prop_test_results)) 

} 

# suppressWarnings(rm(t_test_results, prop_test_results)) 

percent_ttest_proptest_sim(iter = 10, 
                           exposed_n = 190000, 
                           control_n = 10000, 
                           baseline_rate = 61, 
                           exposed_rate  = 63, 
                           stddev = 0.5, 
                           output_df_name = 'ugc_latam') 

t_test_results_ugc_latam %>% dim 


options(repr.plot.width=800, repr.plot.height=400)

t_test_results_CIs <- 
  t_test_results %>%
  select(conf.low, conf.high) %>%
  pivot_longer(cols=c(conf.low, conf.high), 
                      names_to='confidence_intervals',
                      values_to='values') %>%
  mutate(test = 't.test')    

prop_test_results_CIs <- 
  prop_test_results %>%
  select(conf.low, conf.high) %>%
  pivot_longer(cols=c(conf.low, conf.high), 
                      names_to='confidence_intervals',
                      values_to='values') %>%
  mutate(test = 'prop.test') 

tests_CIs <- rbind(t_test_results_CIs, prop_test_results_CIs)    

t_test_ci_plot <- 
  tests_CIs %>%
  ggplot() + 
    aes(x = confidence_intervals, y = values, color = test) + 
    facet_wrap(~test, scales='free') + 
    geom_boxplot(color = 'black') +
    geom_jitter(alpha = 0.2) + 
    scale_y_continuous(labels = scales::percent) + # breaks=c(.2, .5, .8), limits= c(0, .4), 
    labs(title = 't-test Confidence Intervals',
        subtitle = 'For 1K Simualted New User Retention Rate',
        caption = 'Notes: 
                      sample sizes: exposed = 190000, control = 10000
                      exposed_retention_rate <- rnorm(1, mean=62, sd=3) 
                      control_retention_rate <- rnorm(1, mean=61, sd=3)') 

suppressWarnings(print(t_test_ci_plot))  


options(repr.plot.width=800, repr.plot.height=400)

t_test_pvalues <- 
  t_test_results %>%  
  select(p.value) %>%
  mutate(test = 't.test')
  
prop_test_pvalues <-   
  prop_test_results %>%
  select(p.value) %>%
  mutate(test = 'prop.test') 

tests_pvalues <- rbind(t_test_pvalues, prop_test_pvalues)    

t_test_pvalues_plot <- 
  tests_pvalues %>% 
  ggplot() + 
    aes(x = '', y = p.value, color = test) + 
    facet_wrap(~test, scales='free') +    
    geom_boxplot(color = 'black') +    
    geom_jitter(alpha = 0.2) + 
    scale_y_continuous(labels = scales::percent) + 
    labs(title = 't-test p-values',
        subtitle = 'For Simualted New User Retention Rate',
          caption = 'Notes: 
                        sample sizes: exposed = 190000, control = 10000
                        exposed_retention_rate <- rnorm(1, mean=62, sd=3) 
                        control_retention_rate <- rnorm(1, mean=61, sd=3)') 

suppressWarnings(print(t_test_pvalues_plot)) 

tests_pvalues %>%
  group_by(test) %>%
  # get_summary_stats(p.value) 
  summarise(avg = mean(p.value), 
            se  = sem(p.value), 
            sd  = sd(p.value))

pval_summary_plt <- 
  tests_pvalues %>%
  ggplot() +
    aes(x = test, 
        y = p.value,
        group = test,
        color = test) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    stat_summary(fun.data = "mean_se", geom = "errorbar") + 
    scale_y_continuous(labels = scales::percent) + # , limits = c(0, 0.002)
      labs(title = 'Mean & SE for p-values',
          subtitle = 'For Simualted New User Retention Rate',
            caption = 'Notes: 
                          sample sizes: exposed = 190000, control = 10000
                          exposed_retention_rate <- rnorm(1, mean=62, sd=3) 
                          control_retention_rate <- rnorm(1, mean=61, sd=3)') 

  suppressMessages(print(pval_summary_plt))


# simulate different retention rates 
suppressWarnings(rm(t_test_results_2, prop_test_results_2)) 

exposed_n <- 190000
control_n <- 10000
retention_rates <- seq(from=10, to=90, by=10)

for(ren_rate in retention_rates) {
  # print(ren_rate)

  for(x in 1:1000) {

    # simulate scalar value 
    exposed_retention_rate <- runif(n = 1, min = ren_rate-3, max = ren_rate+3) / 100
    control_retention_rate <- runif(n = 1, min = ren_rate-3, max = ren_rate+3) / 100 

    # generate sequence 
    exposed <- rbinom(n = exposed_n, size = 1, prob = exposed_retention_rate)
    control <- rbinom(n = control_n, size = 1, prob = control_retention_rate)

    # run tests and store results 
    t_test_output <- tidy(t.test(exposed, control)) %>%
                     mutate(retention_rate = ren_rate)

    prop_test_output <- tidy(prop.test(x = c(exposed_retention_rate * exposed_n, 
                                             control_retention_rate * control_n),
                                       n = c(exposed_n, control_n))) %>%
                        mutate(retention_rate = ren_rate)

    if(!exists("t_test_results_2")) { 
      t_test_results_2 <- t_test_output    
    } else {
      t_test_results_2 <- bind_rows(t_test_results_2, t_test_output)
    }

    if(!exists("prop_test_results_2")) { 
      prop_test_results_2 <- prop_test_output    
    } else {
      prop_test_results_2 <- bind_rows(prop_test_results_2, prop_test_output)
    }  

  } 
} 

t_test_results_2 <- t_test_results_2 %>%
                    rename(mean_diff   = estimate, 
                          x_mean      = estimate1,
                          y_mean      = estimate2,
                          t_statistic = statistic)

prop_test_results_2 <- prop_test_results_2 %>%
                      rename(x_prop    = estimate1,
                              y_prop    = estimate2,
                              X_squared = statistic,
                              df        = parameter)           



options(repr.plot.width=800, repr.plot.height=400)

t_test_results_2_CIs <- 
  t_test_results_2 %>% 
  select(retention_rate, conf.low, conf.high) %>%
  pivot_longer(!retention_rate, 
                names_to='confidence_intervals',
                values_to='values') %>%
  mutate(test = 't.test') 

prop_test_results_2_CIs <- 
  prop_test_results_2 %>%
  select(retention_rate, conf.low, conf.high) %>%
  pivot_longer(!retention_rate, 
                names_to='confidence_intervals',
                values_to='values') %>%
  mutate(test = 'prop.test') 

tests_2_CIs <- rbind(t_test_results_2_CIs, prop_test_results_2_CIs)    

t_test_2_ci_plot <- 
  tests_2_CIs %>%
  ggplot() + 
    aes(x = confidence_intervals, y = values, color = test) + 
    facet_grid(cols = vars(retention_rate),
               rows = vars(test)) + 
    geom_boxplot(color = 'black') +
    geom_jitter(alpha = 0.03) + 
    scale_y_continuous(labels = scales::percent) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position="none") + 
    labs(title = 't-test & prop.test Confidence Intervals',
        subtitle = 'For 1K Simualted New User Retention Rate',
        caption = 'Notes: 
                      sample sizes: exposed = 190000, control = 10000
                      exposed_retention_rate <- rnorm(1, mean=62, sd=3) 
                      control_retention_rate <- rnorm(1, mean=61, sd=3)') 

suppressWarnings(print(t_test_2_ci_plot))  


bind_rows(
  t_test_results_2 %>%    
    group_by(retention_rate) %>% 
    summarise(avg = mean(p.value), 
              se  = sem(p.value), 
              sd  = sd(p.value)) %>%
    mutate(test = 't.test')
    , 
    prop_test_results_2 %>%    
    group_by(retention_rate) %>% 
    summarise(avg = mean(p.value), 
              se  = sem(p.value), 
              sd  = sd(p.value)) %>%
    mutate(test = 'prop.test')
  ) %>%  
ggplot() + 
aes(x = retention_rate, y = avg, color = test) + 
geom_point(position=position_dodge(width = 5)) +
geom_errorbar(aes(ymin = avg-se, ymax = avg+se), width=.2,
                  position = position_dodge(width = 5)) + 
scale_y_continuous(labels = scales::percent, limits= c(0, 0.2)) +                   
scale_x_continuous(breaks = seq(10, 90, 10)) +       
labs(title = 't-test & prop.test Confidence Intervals
By Retention Rate',
    subtitle = 'For 1K Simualted New User Retention Rate',
    caption = 'Notes:',
    y = 'average',
    x = 'retention rate')





