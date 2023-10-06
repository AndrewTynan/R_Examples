
group_percents <- function(data,by) {
  
  if(!"scales" %in% (.packages())){ # load if not in session
    library(scales)
  }
  
  data %>% 
    group_by({{by}}) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(percent = scales::percent(round(n/sum(n),2)))
  
}

mtcars %>% group_percents(., cyl)

