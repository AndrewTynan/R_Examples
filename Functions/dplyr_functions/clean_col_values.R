
clean_col_values <- function(df, metric_cols = 'Metric') {
  
  #' @description 
  #' Provides a way to clean the values in a data.frame column containing string values.
  #' The use case is cleaning values before reporting in a table (specifically one created using skim() where the variable name is displayed).
  #' Cleaning involves replacing underscores with spaces and capitalizing the first letter in each portion of the string following a space.
  #'
  #' @param df the data.frame
  #' @param metric_cols the column(s) in the data.frame that will be cleaned
  
  df %>% 
    mutate_at(vars(matches( {{metric_cols}} )), gsub, pattern = '[_]+[.]+["]+', replacement = " ") %>% 
    mutate_at(vars(matches( {{metric_cols}} )), stringr::str_to_title)
  
}
