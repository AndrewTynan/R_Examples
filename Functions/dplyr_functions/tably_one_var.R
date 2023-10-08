
tably_one_var <- function(data, var) { 
  
    library(janitor)
    
    data %>% 
      janitor::tabyl({{var}}) %>% 
      janitor::adorn_totals("row") %>% 
      janitor::adorn_pct_formatting()
  
}

# example
mtcars %>% tably_one_var(., cyl)

