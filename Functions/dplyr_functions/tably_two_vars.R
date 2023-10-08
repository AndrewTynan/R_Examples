
tably_two_vars <- function(data, var1, var2, with_percents_by=NA, perc_decimals=1) { 
  
  # Note: can only use perc_decimals arg when with_percents_by arg is used. Might want to change this.. 
  
  library(janitor)
  
  data %>% 
    janitor::tabyl({{var1}}, {{var2}}) %>% 
    janitor::adorn_totals("row", "col") %>% 
    janitor::adorn_title("combined") %>% 
    {if(is.na({{with_percents_by}} )) {
      (.) %>% 
        dplyr::mutate_if(is.numeric, format, big.mark = ',')
      
    } else if(!is.na({{with_percents_by}})) {
      (.) %>% 
        janitor::adorn_percentages(with_percents_by) %>% 
        janitor::adorn_pct_formatting(digits = perc_decimals) %>% 
        janitor::adorn_ns(position = 'front')
    } 
    }
} 


# examples 
mtcars %>% tably_two_vars(., cyl, carb)

mtcars %>% tably_two_vars(., cyl, carb, with_percents_by = 'row')

mtcars %>% tably_two_vars(., cyl, carb, with_percents_by = 'col')

mtcars %>% tably_two_vars(., cyl, carb, with_percents_by = 'all')
