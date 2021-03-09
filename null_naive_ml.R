library(tidyverse)
library(tidymodels)

amenities <- read.csv("amenities.csv")
sale_listings <- read.csv("sale_listings.csv")

null_model <- mean(sale_listings$price)

train <- sale_listings %>% sample_frac(.70)
test <- sale_listings %>% anti_join(train)