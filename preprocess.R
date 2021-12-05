library(tidyverse)

feature <- read_csv("src/training_set_features.csv")

labels <- read_csv("src/training_set_labels.csv")


df <- merge(
  feature,
  labels,
  by = "respondent_id"
)

df$respondent_id <- NULL

df %>% write_csv(., "src/train.csv")

