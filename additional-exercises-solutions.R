



# Exercise 1
welcome_message <- function() {
  print("Welcome to MMES-565!")
}

# Exercise 2
greet_user <- function(name) {
  glue::glue("Hello {name}, welcome to MMES-565!")
}

# Exercise 3
double_number <- function(x) {
  x * 2
}

# Exercise 4
add_and_multiply <- function(a, b) {
  (a + b) * 10
}

# Exercise 5
make_summary_tibble <- function(a, b, c) {
  tibble(mean_value = mean(c(a, b, c)), sum_value = sum(a, b, c))
}

# Exercise 6
convert_to_celsius <- function(fahrenheit = 98.6) {
  (fahrenheit - 32) * (5 / 9)
}

# Exercise 7
square_and_double <- function(x) {
  double_number(x^2)
}

# --- Part 2 Solutions ---
library(tidyverse)

data <- read_csv("data/drto_psu_denisty.csv")

# Exercise 8
protection_label <- function(df) {
  df <- df %>% mutate(PROT_LABEL =
  case_when(
    PROT == 0 ~ "Open",
    PROT == 1 ~ "Northeast Reserve",
    PROT == 2 ~ "National Park",
    TRUE ~ NA_character_
  ))
}

# Exercise 9
filter_by_species <- function(df, species_code) {
  df %>% filter(SPECIES_CD == species_code)
}

# Exercise 10
mean_density_by_prot <- function(df) {
  df %>% group_by(PROT_LABEL) %>% 
    summarise(mean_density = mean(density, na.rm = TRUE), .groups = 'drop')
}

# Exercise 11
mean_density_by <- function(df, group_col) {
  df %>% group_by( {{group_col}} ) %>% 
    summarise(mean_density = mean(density, na.rm = TRUE), .groups = 'drop')
}

# Exercise 12
density_summary <- function(df) {
  df %>% summarise(
    mean_density = mean(density, na.rm = TRUE),
    median_density = median(density, na.rm = TRUE),
    sd_density = sd(density, na.rm = TRUE)
  )
}

# Exercise 13
species_density_summary <- function(df, species_code) {
  df %>% 
    filter_by_species(species_code) %>% 
    mean_density_by_prot(.)
}

# Exercise 14
compare_species_density <- function(df, species_code) {
  df %>% 
    filter_by_species(species_code) %>% 
    mean_density_by_prot() %>% 
    ggplot(aes(x = PROT_LABEL, y = mean_density, fill = PROT_LABEL)) + 
    geom_col() + 
    labs(
    title = paste("Mean Density for", species_code),
    x = "Protection Zone",
    y = "Mean Density"
  ) + 
    theme_minimal()
}

# Exercise 15
species_density_by_prot <- function(df, species_code, protection = NULL) {
  df_filtered <- df %>% 
    filter_by_species(species_code)
  
  if (!is.null(protection)) {
    df_filtered %>%
      filter(PROT %in% protection) %>% 
      mean_density_by_prot()
  } else {
    df_filtered %>%
      mean_density_by_prot()
  }
}
