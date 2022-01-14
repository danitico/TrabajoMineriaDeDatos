source("src/utils/functions.R")
library(tidyverse)
library(e1071)
library(AUC)

dftest <- read_dataset(
  "src/data/test_imputed.csv"
)

df <- read_dataset(
  "src/data/train_imputed.csv"
)

df_one_target <- collapse_labels(df, 36, 37)
df_one_target$target = factor(df_one_target$target)

best_feature_h1n1 <- df %>% select(
  behavioral_antiviral_meds,
  doctor_recc_h1n1,
  health_insurance,
  opinion_h1n1_vacc_effective,
  opinion_h1n1_risk,
  age_group,
  race,
  hhs_geo_region,
  census_msa,
  household_adults,
  household_children,
  employment_industry,
  h1n1_vaccine
)

set.seed(42)

train_bagging <- function (classifier, number_classifiers, x, y) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(x)[1], replace = T, size = dim(x)[1])
      classifier(x[samples,], y[samples])
    }
  )
}

predict_bagging <- function(models, test, n_classes) {
  apply(
    array(
      unlist(
        lapply(
          1:length(models),
          function(i) {
            predict(models[[i]], newdata = test, type="raw")
          }
        )
      ),
      dim = c(dim(test)[1], n_classes, length(models))
    ),
    c(1, 2),
    mean
  )
}

# Etiquetas separadas

models_h1n1 <- train_bagging(
  naiveBayes,
  3,
  best_feature_h1n1 %>% select(-h1n1_vaccine),
  best_feature_h1n1$h1n1_vaccine
)

preds_h1n1 <- predict_bagging(
  models_h1n1,
  dftest %>% select(
    behavioral_antiviral_meds,
    doctor_recc_h1n1,
    health_insurance,
    opinion_h1n1_vacc_effective,
    opinion_h1n1_risk,
    age_group,
    race,
    hhs_geo_region,
    census_msa,
    household_adults,
    household_children,
    employment_industry
  ),
  2
)


best_feature_seasonal <- df %>% select(
  h1n1_knowledge,
  behavioral_antiviral_meds,
  doctor_recc_seasonal,
  health_worker,
  health_insurance,
  opinion_seas_vacc_effective,
  opinion_seas_risk,
  opinion_seas_sick_from_vacc,
  age_group,
  education,
  race,
  hhs_geo_region,
  employment_industry,
  seasonal_vaccine
)

models_seasonal <- train_bagging(
  naiveBayes,
  3,
  best_feature_seasonal %>% select(-seasonal_vaccine),
  best_feature_seasonal$seasonal_vaccine
)

preds_seasonal <- predict_bagging(
  models_seasonal,
  dftest %>% select(
    h1n1_knowledge,
    behavioral_antiviral_meds,
    doctor_recc_seasonal,
    health_worker,
    health_insurance,
    opinion_seas_vacc_effective,
    opinion_seas_risk,
    opinion_seas_sick_from_vacc,
    age_group,
    education,
    race,
    hhs_geo_region,
    employment_industry,
  ),
  2
)

submission <- get_submission_dataframe(
  preds_h1n1[, 2],
  preds_seasonal[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_h1n1_and_seasonal_and_bagging_with_3_classifiers.csv"
)

# etiquetas juntas

model_one_target <- train_bagging(
  naiveBayes,
  10,
  df_one_target %>% select(
    h1n1_knowledge,
    doctor_recc_h1n1,
    doctor_recc_seasonal,
    chronic_med_condition,
    health_worker,
    health_insurance,
    opinion_h1n1_vacc_effective,
    opinion_h1n1_risk,
    opinion_seas_vacc_effective,
    opinion_seas_risk,
    opinion_seas_sick_from_vacc,
    age_group,
    education,
    race,
    rent_or_own,
    hhs_geo_region,
    employment_industry
  ),
  df_one_target$target
)

preds_one_target <- predict_bagging(
  model_one_target,
  dftest %>% select(
    h1n1_knowledge,
    doctor_recc_h1n1,
    doctor_recc_seasonal,
    chronic_med_condition,
    health_worker,
    health_insurance,
    opinion_h1n1_vacc_effective,
    opinion_h1n1_risk,
    opinion_seas_vacc_effective,
    opinion_seas_risk,
    opinion_seas_sick_from_vacc,
    age_group,
    education,
    race,
    rent_or_own,
    hhs_geo_region,
    employment_industry
  ),
  4
)

preds_one_target_processed <- cbind(
  preds_one_target[,3] + preds_one_target[,4],
  preds_one_target[,2] + preds_one_target[,4]
)

submission <- get_submission_dataframe(
  preds_one_target_processed[, 1],
  preds_one_target_processed[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_unique_label_and_bagging_with_10_classifiers.csv"
)

