# Text processing

library(tidyverse)
library(readr)
library(stringr)
library(tidytext)

text_var <- function(){
#for listings that have no description, force NA to 0 in order to keep all listings
sale_listings_ss <<- sale_listings_imputed %>%
  mutate(listing_description = ifelse(is.na(listing_description) == TRUE, 0, listing_description)) %>%
  mutate(
    stainless_steel = as.integer(str_detect(listing_description, "[Ss]tainless.[Ss]teel")),
    hw_floors = as.integer(str_detect(listing_description, "[Hh]ardwood.[Ff]loors?")),
    wd = as.integer(str_detect(listing_description, c(
      "[Ww]d",
      "[Ww]/d",
      "[Ww]&d",
      "[Ww] & d",
      "[Ww]asher/dryer",
      "[Ww]asher / dryer",
      "[Ww]asher and dryer",
      "[Ww]asher&dryer",
      "[Ww]asher & dryer",
      "[Ww]asher-dryer",
      "[Ww]asher dryer"
    ))),
    steel_app = as.integer(str_detect(listing_description, "[Ss]teel.[Aa]ppliances?")),
    fitness = as.integer(str_detect(listing_description, c(
      "[Ff]itness",
      "[Ff]itness center",
      "[Ff]itness-center"
    ))),
    
    # top 10 words with highest price avergae and highest frequency
    marble = as.integer(str_detect(listing_description, "[Mm]arble")),
    master = as.integer(str_detect(listing_description, "[Mm]aster")),
    views = as.integer(str_detect(listing_description, "[Vv]iews")),
    custom = as.integer(str_detect(listing_description, "[Cc]ustom")),
    floor = as.integer(str_detect(listing_description, "[Ff]loor")),
    private = as.integer(str_detect(listing_description, "[Pp]rivate")),
    windows = as.integer(str_detect(listing_description, "[Ww]indows")),
    dining = as.integer(str_detect(listing_description, "[Dd]ining")),
    offers = as.integer(str_detect(listing_description, "[Oo]ffers")),
    light = as.integer(str_detect(listing_description, "[Ll]ight")),
    #----------------------------------------------------------------
    renovate = as.integer(str_detect(listing_description, "[Rr]enovat*")),
    closet_space = as.integer(str_detect(listing_description, "[Cc]loset.[Ss]pace")),
    spacious = as.integer(str_detect(listing_description, "[Ss]pacious*")),
    storage = as.integer(str_detect(listing_description, "[Ss]torage")),
    closet_space = as.integer(str_detect(listing_description, "[Cc]loset.[Ss]pace")),
    roof_deck = as.integer(str_detect(listing_description, "[Rr]oof.[Dd]eck")),
    park = as.integer(str_detect(listing_description, "[Pp]ark")),
    balcony = as.integer(str_detect(listing_description, "[Bb]alcony")),
    courtyard = as.integer(str_detect(listing_description, "[Cc]ourtyard")),
    view = as.integer(str_detect(listing_description, "[Vv]iew*")),
    window = as.integer(str_detect(listing_description, "[Ww]indow*")),
    natural_light = as.integer(str_detect(listing_description, "[Nn]atural.[Ll]ight")),
    en_suite = as.integer(str_detect(listing_description, "[Ee]n.[Ss]uite")),
    pet_friendly = as.integer(str_detect(listing_description, "[Pp]et.[Ff]riendly")),
    tree_lined = as.integer(str_detect(listing_description, "[Tt]ree.[Ll]ined")),
    central_park = as.integer(str_detect(listing_description, "[Cc]entral.[Pp]ark")),
    outdoor_space = as.integer(ifelse((park | balcony | courtyard | roof_deck | central_park | tree_lined) == 1, 1, 0))                              
  )

  library(tidytext)
  afinn_lex <- get_sentiments("afinn")
  sentiments <- sale_listings_ss %>% 
    select(id, listing_description) %>% 
    unnest_tokens(output = word,
                  input = listing_description) %>% 
    anti_join(stop_words) %>% 
    inner_join(afinn_lex) 
  
  sentiment_summary <- sentiments %>% 
    group_by(id) %>% 
    summarise(score = sum(value)) 
  
  sale_listings_ss <<- left_join(sale_listings_ss, sentiment_summary, by = "id")
  #for listings that have no emotion-associated words, use score = 0
  sale_listings_ss <<- sale_listings_ss %>%
    mutate(score = ifelse(is.na(score) == TRUE, 0, score))
}

