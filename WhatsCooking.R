library(jsonlite)
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(ranger)

#import data
train_raw <- fromJSON("./train.json") %>% as_tibble()
test_raw  <- fromJSON("./test.json") %>% as_tibble()

#Preprocessing
train_data <- train_raw %>%
  mutate(ingredients_text = map_chr(ingredients, paste, collapse = " ")) %>%
  select(id, cuisine, ingredients_text)

test_data <- test_raw %>%
  mutate(ingredients_text = map_chr(ingredients, paste, collapse = " ")) %>%
  select(id, ingredients_text)

#Recipe
cuisine_recipe <- recipe(cuisine ~ ingredients_text, data = train_data) %>%
  
  #Split text into individual ingredients
  step_tokenize(ingredients_text) %>%
  
  #Keep the top 1000 most frequent ingredients
  step_tokenfilter(ingredients_text, max_tokens = 1000) %>%
  
  #TFIDF
  step_tfidf(ingredients_text) %>%
  
  #Normalize
  step_normalize(all_numeric_predictors())

#Random Forest Model
rf_spec <- rand_forest(
  trees = 500,
  min_n = 5
) %>%
  set_engine("ranger",
             importance = "impurity") %>%
  set_mode("classification")

#Workflow
cuisine_wf <- workflow() %>%
  add_recipe(cuisine_recipe) %>%
  add_model(rf_spec)

#Fit Model
final_fit <- cuisine_wf %>%
  fit(data = train_data)

#Predictions
predictions <- predict(final_fit, new_data = test_data)

#Submission
submission <- tibble(
  id = test_data$id,
  cuisine = predictions$.pred_class
)

#Save to CSV
write_csv(submission, "submission_cuisine_rf.csv")