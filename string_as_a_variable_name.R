
# https://gist.github.com/andrewheiss/9ce77269f214cfbdcf5a3a40d1697e27


# NOTE: just needed to convert the = to := 

# Expected result
make_data("thing")
#> # A tibble: 2 × 1
#>   thing
#>   <int>
#> 1     1
#> 2     2


# now this works 
make_data <- function(var_name) {
  tibble({{var_name}} := 1:2)
}

make_data("thing")


# now this works 
make_data <- function(var_name) {
  tibble(!!var_name := 1:2)
}

make_data("thing")


# now this works 
make_data <- function(var_name) {
  tibble(!!enquo(var_name) := 1:2)
}

make_data("thing")


# added another approach 
make_data <- function(var_name) {
  tibble("{var_name}" := 1:2)
}

make_data("thing")

