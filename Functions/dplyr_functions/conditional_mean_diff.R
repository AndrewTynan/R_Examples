
conditional_mean_diff  <- function(data, var_1, var_2, cond) {
  
  data %>% 
    group_by(Date) %>%
    dplyr::summarise("{{var_1}}_{{cond}}_{{var_2}}_{{cond}}_mean_diff" := mean( {{var_1}} [ {{cond}} ] - {{var_2}} [ {{cond}} ] ))
  
}
