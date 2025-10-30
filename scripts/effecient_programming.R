########################
#Gillian Coleman
#10/30/2025
#Effecient Programming
########################

#Additional Function Exercises

#load libraries
library (tidyverse)
library(glue)
#Exercise 1
#Create a function welcome_user() that prints a welcome message.

welcome_mes <- function() {
  cat("Welcome to Class!\n")
}

# To use the function:
welcome_mes()

#Exercise 2
#Write a function greet_user() that takes a user’s name and prints a personalized message.

#Hint: look at the tidyverse glue function for using ‘string literals’

greet_user <- function(name){
  
  glue("Welcome {name}!\n")
}
greet_user("Gillian")


#Exercise 3
#Write a function double_number() that takes a numeric input and returns double its value.

double_number <- function(number){
  number * 2
}
double_number(34)


#Exercise 4
#Write a function add_and_multiply() that takes two numbers (a, b), adds them, and multiplies the result by 10.

add_and_multiply <- function(num1,num2){
  (num1 + num2) * 10
}

add_and_multiply(2,3)


#Exercise 5
#Write a function make_summary_tibble() that takes three numeric inputs and returns a dataframe showing their mean and sum.

make_summary_tibble <- function(num1, num2, num3){
  sums <- sum(c(num1, num2, num3))
  mean <- mean(c(num1, num2, num3))
  summary_data <- tibble(
    Statistic = c("Sum", "Mean"),
    Value = c(sums, mean)
  )
  
  return(summary_data)
}

make_summary_tibble(2,3,2)


#Exercise 6
#Write a function convert_to_celsius() that converts Fahrenheit to Celsius. Default argument: fahrenheit = 98.6.

convert_to_celcius <- function(celsius){
  fahrenheit <- (celsius * 9/5) + 32
return(fahrenheit)}

convert_to_celcius(32)


#Exercise 7
#Write a function square_and_double() that uses one of your previous functions to square a number and then double it.


square_and_double <- function(number) {
  result <- double_number(number^2)  # call existing function
  return(result)
}

square_and_double(3)

#Exercise 8
#Write a function protection_label() that converts numeric PROT codes and adds a new column to the dataset called PROT_LABEL.

PSU <- read_csv("data/drto_psu_denisty.csv")

protection_label <- function(data){
  new_data <- data %>%
    mutate(
      PROT_LABEL = case_when( PROT == 0 ~ "Open",
                               PROT == 1 ~ "Northeast Reserve",
                               PROT== 2 ~ "National Park")
    
)
  
  return(new_data)
}

PSU <- protection_label(PSU)


#Exercise 9
#Write a function filter_by_species() that filters the dataset for a given species code.

filter_by_species <- function(data, species){
  filter(data, species == SPECIES_CD)
  
}

filter_by_species(PSU, "ABL HIAN")  


#Exercise 10
#Write a function mean_density_by_prot() that calculates the mean density by protection zone.

mean_density_by_prot <- function(data){
  data %>%
    group_by(PROT) %>%
    summarise(mean_den = mean(density, na.rm = TRUE)) %>%
    arrange(PROT)
}

mean_density_by_prot(PSU) 


#Exercise 11
#Write a function mean_density_by() that takes a dataframe and a column name and calculates mean density grouped by that column

mean_density_by <- function(data, column){
  data %>%
    group_by({{column}}) %>%
    summarise(mean_den = mean(density, na.rm = TRUE))
}

mean_density_by(PSU, STRAT)

#Exercise 12
#Write a function density_summary() that returns mean, median, and standard deviation of density.

density_summary<- function(data){
  data %>%
    summarise(
      mean_den = mean(density, na.rm = TRUE),
      med = median(density, na.rm = TRUE),
      std_dev = sd(density, na.rm = TRUE)
    )
}

density_summary(PSU)


#Exercise 13
#Write a function species_density_summary() that:

#1. Calls filter_by_species().
#2. Uses mean_density_by_prot().
#3. Returns a dataframe summarizing mean density by protection zone.

species_density_summary <- function(data, species){
  filter_by_species(species==SPECIES_CD)
  mean_density_by_prot()
  summary_data <- tibble(
    Statistic = c("Mean"),
    Value = c(mean_density_by_prot()))}

species_density_summary(PSU, EPI GUT)
