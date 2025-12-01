library(jsonlite)
library(dplyr)
library(purrr)  
library(stringr)

train <- fromJSON('./train.json')
test <- fromJSON('./test.json')

train_features <- train %>%
  mutate(
    #Number of Ingredients
    n_ingredients = map_int(ingredients, length),
    
    #Average Ingredient Name Length
    avg_ing_len = map_dbl(ingredients, ~ mean(nchar(.x))),
    
    #Soy Flag
    has_soy = map_int(ingredients, ~ as.integer(any(str_detect(.x, "soy"))))
  )

# 3. Preview the new features
head(train_features %>% select(cuisine, n_ingredients, avg_ing_len, has_soy))
