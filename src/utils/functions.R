library(tidyverse)

### COLLAPSING LABELS

collapse_labels = function(dataframe, col_index_1, col_index_2) {
  
  collapse_two_binaries = function(x, y) {
    paste(x,y, sep="")
  }
  
  new_dataframe = dataframe[,-c(col_index_1, col_index_2)] %>% as.data.frame()
  colnames(new_dataframe) = colnames(dataframe)[-c(col_index_1, col_index_2)]
  
  new_dataframe$target = collapse_two_binaries(
    dataframe[,col_index_1],
    dataframe[,col_index_2]
  )
  return(new_dataframe)
}


### SPLITTING LABELS

split_target = function(probs_dataframe) {
  # Assumed probs_dataframe has a row per instance and is of the form (id, prob(0,0), prob(0,1), prob(1,0), prob(1,1)).
  
  new_dataframe = apply(probs_dataframe, 1, function(row) c(
      row[1], # id
      row[4]+row[5], # p(10)+p(11)
      row[3]+row[5]  # p(01)+(11)
    )) %>% t()
    
  colnames(new_dataframe) = c("respondent_id", "h1n1_vaccine", "seasonal_vaccine")
  return(new_dataframe)
}


get_unified_training_dataset <- function () {
  feature <- read_csv("src/data/drivendata/training_set_features.csv")
  labels <- read_csv("src/data/drivendata/training_set_labels.csv")
  
  # Merging dataframes
  df <- merge(
    feature,
    labels,
    by = "respondent_id"
  )

  # Deleting id variable
  df$respondent_id <- NULL
  
  df %>% write_csv("src/data/drivendata/train.csv")
}

get_test_dataset <- function () {
  df <- read_csv("src/data/drivendata/test_set_features.csv")
  
  df$respondent_id <- NULL
  
  df %>% write_csv("src/data/drivendata/test.csv")
}

read_dataset <- function(file) {
  df <- read_csv(file)
  
  df <- df %>% mutate(
    across(
      everything(),
      ~ factor(.x)
    )
  )
  
  df
}

get_submission_dataframe <- function(h1n1_vaccine_probs, seasonal_vaccine_probs) {
  submission <- read_csv("src/data/drivendata/submission_format.csv")
  
  submission$h1n1_vaccine <- h1n1_vaccine_probs
  submission$seasonal_vaccine <- seasonal_vaccine_probs
  
  submission
}
