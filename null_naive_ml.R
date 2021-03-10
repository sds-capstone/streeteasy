library(tidyverse)
library(tidymodels)

amenities <- read.csv("amenities.csv")
sale_listings <- read.csv("sale_listings.csv")

# null_model <- mean(sale_listings$price)

# Create test & training sets
set.seed(410)
train <- sale_listings %>% sample_frac(.70)
test <- sale_listings %>% anti_join(train)

# log transformation
train_log10 <- train %>%
  mutate(log10_price = log10(price),
         log10_size_sqft = log10(size_sqft))
test_log10 <- test %>%
  mutate(log10_size_sqft = log10(size_sqft))

# Model 1: Null model
null_model_formula <- as.formula("log10_price ~ 1")
null_model <- lm(null_model_formula, data=train_log10)
predicted_points_null <- null_model %>%
  broom::augment(newdata = test_log10)

test_log10 <- test_log10 %>% 
  mutate(
    log10_price_hat = predicted_points_null$.fitted,
    price_hat = 10^log10_price_hat)

# Compute RMSLE
rmsle_hat_null <- RMSLE(test_log10$price_hat, test_log10$price)
rmsle_hat_null
