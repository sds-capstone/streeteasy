# Pre-processing

library(tidyverse)
library(readr)

# To use this script:
# source("pre-processing.R")

load_data <- function(){
  # Load the data in global environment
  sale_listings <<- read_csv("data/sale_listings.csv") 
  amenities <<- read_csv("data/amenities.csv")
}

clean_data <- function(){

  sale_listings <<- sale_listings %>%
    # Make state names consistent
    mutate(addr_state = ifelse(addr_state == "New Jersey", "NJ", 
                               ifelse(addr_state == "New York", "NY", addr_state))) %>%
    # Make sure we have valid prices
    filter(price != 0)
  
  sale_listings <<- sale_listings %>%
    # Reasonable range of sale prices
    filter(price < quantile(price,0.95))
}