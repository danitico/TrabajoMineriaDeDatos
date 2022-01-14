library(mice)
library(e1071)
library(tidyverse)
library(AUC)

df <- read_dataset("src/data/train_imputed.csv")
dftest <- read_dataset("src/data/test_imputed.csv")


imputeTrain <- mice(df[,-c(36, 37)], maxit = 10, m=1, method = "polr", nnet.MaxNWts = 3000)
imputeTest <- mice(dftest, maxit=10, m=1, method = "polr", nnet.MaxNWts = 3000)


imputed_train <- complete(imputeTrain)
imputed_train %>% write_csv("src/data/train_all_imputed.csv")

imputed_test <- complete(imputeTest)
imputed_test %>% write_csv("src/data/test_all_imputed.csv")
