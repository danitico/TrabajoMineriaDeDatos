source("src/utils/functions.R")
source("src/utils/preprocess.R")

features_test <- read_dataset("src/data/drivendata/test.csv") %>% 
  input_unemployed_occupation_and_industry() %>% 
  input_extreme_health_insurance() %>% 
  input_vaccine_reccomendation() %>% 
  input_unemployed_with_common_na() %>%
  misc_changes() %>%
  no_income_and_home() %>%
  write_csv("src/data/test_imputed.csv")
