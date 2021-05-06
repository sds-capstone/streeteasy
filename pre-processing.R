# Pre-processing

library(tidyverse)
library(readr)
library(stringr)


# To use this script:
# source("pre-processing.R")

load_data <- function(){
  # Load the data in global environment
  sale_listings <<- read_csv("data/sale_listings.csv") 
  amenities <<- read_csv("data/amenities.csv")
}

clean_data <- function(){

  sale_listings <<- sale_listings %>%
    # Make sure we have valid prices
    filter(price > 1)%>%
    # Reasonable range of sale prices
    filter(price < quantile(price, 0.95)) %>%
    # Filter out unreasonable numbers 
    filter(size_sqft != 588527) %>%
    filter(bedrooms != 99) %>%
    filter(bathrooms != 66) %>%
    # Make unreasonable sizes NA
    mutate(size_sqft = ifelse(size_sqft < 30, NA, size_sqft))
  
  #import zipcode data 
  library(zipcodeR)
  data("zip_code_db")
  
  zip_code <- zip_code_db %>%
    select(zipcode, major_city, county, state, lat, lng)
  
  sale_listings <<- sale_listings %>%
    mutate(addr_zip = ifelse(addr_zip < 10000, paste0("0", addr_zip), addr_zip)) %>%
    rename("zipcode" = "addr_zip") %>%
    mutate(zipcode = as.character(zipcode))
  
  #join sale_listings with zipcode data
  sale_listings <<- left_join(sale_listings, zip_code, by = "zipcode")
  
  #these two zipcodes did not have info in zip_code_db, so use original ones in sale_listings
  sale_listings <<- sale_listings %>%
    mutate(lat = ifelse(zipcode %in% c("11249", "11466"), addr_lat, lat),
           lng = ifelse(zipcode %in% c("11249", "11466"), addr_lon, lng)) %>%
    select(id, property_id, unittype, listing_description, bedrooms, bathrooms, 
           size_sqft, price, addr_street, addr_unit, zipcode, floor_count, 
           year_built, is_historic, major_city:lng)%>%
    rename(long=lng)
  
  #5763 property_id has duplicates 
  duplicate_id <- sale_listings %>%
    filter(!is.na(property_id)) %>%
    group_by(property_id) %>%
    summarize(n=n()) %>%
    filter(n > 1)
  
  #12,786 listings should eventually only keep 5763
  #make size_sqft 0 == NA 
  duplicate_listing <- sale_listings %>%
    mutate(size_sqft = ifelse(size_sqft == 0, NA, size_sqft)) %>%
    filter(property_id %in% duplicate_id$property_id) 
  
  #count row NA 
  na_row <- as.data.frame(rowSums(is.na(duplicate_listing))) %>%
    rename("na_row" = "rowSums(is.na(duplicate_listing))")
  
  duplicate_na <- cbind.data.frame(duplicate_listing, na_row)
  
  #choose listing that has less NA attributes 
  #11,334 listings 
  duplicate_listing_2 <- duplicate_na %>%
    group_by(property_id) %>%
    filter(na_row == min(na_row)) 
  
  length_des <- as.data.frame(nchar(duplicate_listing_2$listing_description)) %>%
    rename("len_des" = "nchar(duplicate_listing_2$listing_description)") %>%
    mutate(len_des = as.numeric(len_des))
  
  duplicate_des <- cbind.data.frame(duplicate_listing_2, length_des)
  
  #keep the one with longer description
  no_duplicate <- duplicate_des  %>%
    mutate(len_des = ifelse(is.na(len_des) == TRUE, 0, len_des)) %>%
    group_by(property_id) %>%
    filter(len_des == max(len_des)) %>%
    #then keep the one with more bedrooms 
    filter(bedrooms == max(bedrooms)) %>%
    arrange(property_id) %>%
    #lastly keep distinct ids 
    distinct(property_id, .keep_all = TRUE) %>%
    select(-na_row, -len_des)

  #eventually there should be about 52638 listings 
  sale_listings <<- sale_listings %>%
    filter(!property_id %in% duplicate_id$property_id) %>%
    rbind.data.frame(no_duplicate)
  
  sale_listings <<- sale_listings %>%
    filter(!is.na(state))%>%
    mutate(unittype = as.factor(unittype),
           is_historic = as.factor(is_historic),
           major_city = as.factor(major_city),
           county = as.factor(county),
           state = as.factor(state)) %>%
    #we looked at the two listings that have NA in is_historic and they were not historic (built in 2007);
    #resolve NA for random forest models 
    mutate(is_historic = replace_na(is_historic, 0))
  
  # Group cities to reduce number of levels
  # Look at median price of each city
  median_price_city <- aggregate(sale_listings$price, 
                                 by = list(sale_listings$major_city), 
                                 median)
  
  # Create groups based on median prices
  median_price_city$price_group <- cut(median_price_city$x, 10, include.lowest=TRUE,
                                       labels = seq(10,1))
  
  city_group <- median_price_city%>%
    select(-x) %>%
    rename(major_city = Group.1,
           city_group = price_group)
  
  sale_listings <<- left_join(sale_listings, city_group, by = "major_city") %>%
    mutate(city_group = as.factor(city_group))
  
}

impute_data <- function(){
  
  library(mice)
  # Create a new data frame for imputation
  sale_listings_mice <- sale_listings %>%
    mutate(log_size = log(size_sqft))
  
  # Only impute NA values for the log_size column
  na_matrix <- is.na(sale_listings_mice)
  na_matrix[,-ncol(sale_listings_mice)] <- FALSE
    
  # Create a predictor matrix
  # Predictors: "unittype", "bedrooms", "bathrooms", "floor_count", "major_city", "state"
  predictorMatrix <- make.predictorMatrix(sale_listings_mice)
  predictorMatrix[,c(1, 2, 4, 7:11, 13, 14, 16, 18:20)] <- 0
  predictorMatrix[1:19,] <- 0
  
  # Imputation with mice
  sale_listings_imp <- mice(sale_listings_mice, m=1, 
                            method = "norm.predict", 
                            predictorMatrix = predictorMatrix,
                            where = na_matrix,
                            seed = 410)
  
  # Fill in the generated values
  sale_listings_imputed <- complete(sale_listings_imp)
  
  # Clean up and un-log the log_size variable
  sale_listings_imputed <- sale_listings_imputed %>%
    rename(size_sqft_na = size_sqft) %>%
    mutate(size_sqft = exp(log_size)) %>%
    select(-log_size)
  
  return(sale_listings_imputed)
  
}

ml_setup <- function(data){

  #set seed for spliting train/test
  set.seed(410)

  # For now, split the data into training/test sets
  train <<- data %>% sample_frac(.70)
  test <<- data %>% anti_join(train)
  
  # Implement 5-fold CV in the future
  
}
