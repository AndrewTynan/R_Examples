
tably_two_vars <- function(data, var1, var2, with_percents_by=NA, perc_decimals=NA) { 
  
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

mtcars %>% tably_two_vars(., cyl, carb)