
# helper functions 

print_df <- name <- function(x) x %>% print(na.print = "", n = Inf) # print entire data.frame() 

mtcars %>% print_df


print_col_vals <- function(data, col) {
  data %>% dplyr::select( {{col}} ) %>% pull(.) %>% unique(.) %>% sort(.) %>% print()
}

diamonds %>% print_col_vals(., 'color')


