
clean_col_names <- function(df) {
  
  df %>%
    rename_with(., ~gsub('[_]+[.]+["]+'), " ", .X) %>%
    rename_with(., ~stringr::str_to_title(.x))
  
}


