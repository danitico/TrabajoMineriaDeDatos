source("src/utils/functions.R")
source("src/utils/preprocess.R")
library(Amelia)
library(naniar)

features_test <- read_dataset("src/data/drivendata/test.csv") %>% 
  drop_too_empty_columns() %>% # Es el conjunto de test el que determina las columnas que se quitan
  input_unemployed_occupation_and_industry() %>% 
  input_extreme_health_insurance() %>% 
  input_vaccine_reccomendation() %>% 
  input_unemployed_with_common_na()
