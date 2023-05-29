
# https://gist.github.com/andrewheiss/0724b29a8c8b0c3bd0a49b896454be87

library(tidyverse)
library(glue) 

# Manual way that gives you the most control over the areas
variable <- rnorm(1000)
variable_density <- density(variable)

example_df <- tibble(x = variable_density$x, 
                     y = variable_density$y) %>%
              mutate(fill_stuff = case_when(x < -1 ~ "Lower end",
                                            x > 1 ~ "Upper end",
                                            TRUE ~ "Middle area"))


shaded_density_area <- function(data, x_var, y_var) {
  
  x_name <- data %>% dplyr::select({{x_var}}) %>% colnames(.)
  y_name <- data %>% dplyr::select({{y_var}}) %>% colnames(.)
  
  data %>%
    ggplot(., aes(x = {{x_var}}, y = {{y_var}})) +
    geom_line() +
    geom_area(data = filter(data, fill_stuff == "Lower end"),   aes(fill = "Lower end")) +
    geom_area(data = filter(data, fill_stuff == "Upper end"),   aes(fill = "Upper end")) +
    geom_area(data = filter(data, fill_stuff == "Middle area"), aes(fill = "Middle area")) +
    scale_fill_manual(values = c("red", "blue", "darkorange")) + 
    labs(title = glue::glue("A Shaded Density of {x_name} by {y_name}"))
  
}


shaded_density_area(example_df, x, y)
