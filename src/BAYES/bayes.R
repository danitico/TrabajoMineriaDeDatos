source("src/utils/functions.R")
library(tidyverse)
library(e1071)
library(AUC)
library(unbalanced)
library(cobalt)
library(FSelectorRcpp)

df <- read_dataset(
  "src/data/train_imputed.csv"
)

dftest <- read_dataset(
  "src/data/test_imputed.csv"
)


x <- df %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
y1 <- df %>% select(h1n1_vaccine) %>% as.matrix(.)
y2 <- df %>% select(seasonal_vaccine) %>% as.matrix(.)



# Kfold para todas las ejecuciones
kfolds <- get_kfolds(
  df,
  10
)

# Validación cruzada con multiclasificador para los casos 1-6
results <- cross_validation(
  kfolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      h1n1_vaccine~.-seasonal_vaccine,
      data = train_data
    )
    
    model_seasonal <- naiveBayes(
      seasonal_vaccine~.-h1n1_vaccine,
      data = train_data
    )
    
    list(model_h1n1, model_seasonal)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    seasonal_predicition <- predict(
      models[[2]],
      newdata = test_data,
      type = "raw"
    )
    
    auc_h1n1 <- auc(roc(h1n1_predicition[, 2], test_data$h1n1_vaccine))
    auc_seasonal <- auc(roc(seasonal_predicition[, 2], test_data$seasonal_vaccine))
    
    mean(c(auc_h1n1, auc_seasonal))
  }
)


model1 <- naiveBayes(x, df$h1n1_vaccine)
model2 <- naiveBayes(x, df$seasonal_vaccine)
test1 <- predict(model1, newdata = dftest, type="raw")
test2 <- predict(model2, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_with_laplace=5.csv"
)


# Búsqueda de mejores atributos para h1n1_vaccine

evaluator_bayes_h1n1 <- function(attributes, data, dependent = "h1n1_vaccine") {
  model <- naiveBayes(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x <- data %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
  probs <- predict(model, newdata = x, type="raw")
  
  auc(roc(probs[, 2], data$h1n1_vaccine))
}

result <- FSelectorRcpp::feature_search(
  attributes = names(df)[1:35],
  fun = evaluator_bayes_h1n1,
  data = df,
  parallel = T
)

# El resultado que se ha obtenido para predecir h1n1_vaccine es que utilice los siguientes parámetros
# behavioral_antiviral_meds
# doctor_recc_h1n1
# health_insurance
# opinion_h1n1_vacc_effective
# opinion_h1n1_risk
# age_group
# race
# hhs_geo_region
# census_msa
# household_adults
# household_children
# employment_industry

# Validación cruzada para ver como de bueno es la seleccion de características
results <- cross_validation(
  kfolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      h1n1_vaccine~.,
      data = train_data %>% select(
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
    )
    
    model_seasonal <- naiveBayes(
      seasonal_vaccine~.-h1n1_vaccine,
      data = train_data
    )
    
    list(model_h1n1, model_seasonal)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    seasonal_predicition <- predict(
      models[[2]],
      newdata = test_data,
      type = "raw"
    )
    
    auc_h1n1 <- auc(roc(h1n1_predicition[, 2], test_data$h1n1_vaccine))
    auc_seasonal <- auc(roc(seasonal_predicition[, 2], test_data$seasonal_vaccine))
    
    mean(c(auc_h1n1, auc_seasonal))
  }
)

probablyBestModelh1n1 <- naiveBayes(
  h1n1_vaccine~.,
  data = df %>% select(
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
)

test1 <- predict(probablyBestModelh1n1, newdata = dftest, type="raw")
test2 <- predict(model2, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_h1n1.csv"
)



# Búsqueda de mejores atributos para seasonal_vaccine

evaluator_bayes_seasonal <- function(attributes, data, dependent = "seasonal_vaccine") {
  model <- naiveBayes(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x <- data %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
  probs <- predict(model, newdata = x, type="raw")
  
  auc(roc(probs[, 2], data$seasonal_vaccine))
}

result <- FSelectorRcpp::feature_search(
  attributes = names(df)[1:35],
  fun = evaluator_bayes_seasonal,
  data = df,
  parallel = T
)

# El resultado que se ha obtenido para predecir seasonal_vaccine es que utilice los siguientes parámetros
# h1n1_knowledge
# behavioral_antiviral_meds
# doctor_recc_seasonal
# health_worker
# health_insurance
# opinion_seas_vacc_effective
# opinion_seas_risk
# opinion_seas_sick_from_vacc
# age_group
# education
# race
# hhs_geo_region
# employment_industry

# Validación cruzada para ver como de bueno es la seleccion de características
results <- cross_validation(
  kfolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      h1n1_vaccine~.,
      data = train_data %>% select(
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
    )
    
    model_seasonal <- naiveBayes(
      seasonal_vaccine~.,
      data = train_data %>% select(
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
    )
    
    list(model_h1n1, model_seasonal)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    seasonal_predicition <- predict(
      models[[2]],
      newdata = test_data,
      type = "raw"
    )
    
    auc_h1n1 <- auc(roc(h1n1_predicition[, 2], test_data$h1n1_vaccine))
    auc_seasonal <- auc(roc(seasonal_predicition[, 2], test_data$seasonal_vaccine))
    
    mean(c(auc_h1n1, auc_seasonal))
  }
)

probablyBestModelseasonal <- naiveBayes(
  seasonal_vaccine~.,
  data = df %>% select(
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
)


test1 <- predict(probablyBestModelh1n1, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_h1n1_and_seasonal.csv"
)


# smote para h1n1 pero sin seleccion de caracteristicas

smote <- ubSMOTE(x, df$h1n1_vaccine)
smotey <- smote$Y

smoteData <- cbind(smote$X, smotey)


smoteFolds <- get_kfolds(
  smoteData,
  10
)

# validación cruzada para smote
results <- cross_validation(
  smoteFolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      smotey~.,
      data = train_data
    )
    
    list(model_h1n1)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    auc(roc(h1n1_predicition[, 2], test_data$smotey))
  }
)

smoteModel <- naiveBayes(smote$X, smote$Y)
smotePrediction <- predict(smoteModel, newdata = smote$X, type="raw")

test1 <- predict(smoteModel, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_seasonal_and_smote_for_h1n1.csv"
)


# smote para h1n1 pero con seleccion de caracteristicas

# validación cruzada para smote con seleccion de características
results <- cross_validation(
  smoteFolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      smotey~.,
      data = train_data %>% select(
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
        smotey
      )
    )
    
    list(model_h1n1)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    auc(roc(h1n1_predicition[, 2], test_data$smotey))
  }
)

smote$X <- smote$X %>% select(
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
)

smoteModel <- naiveBayes(smote$X, smote$Y)

test1 <- predict(smoteModel, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_h1n1_and_seasonal_and_smote_for_h1n1.csv"
)



# Undersampling de la clase mayoritaria sin seleccion de caracteristicas
onehotdata <- cobalt::splitfactor(x)
onehotdata[is.na(onehotdata)] <- 0

tomekdata <- ubTomek(onehotdata, df$h1n1_vaccine)
tomek_x <- x[-tomekdata$id.rm,]
tomek_y <- df$h1n1_vaccine[-tomekdata$id.rm]

tomekData <- cbind(tomek_x, tomek_y)

tomekFolds <- get_kfolds(
  tomekData,
  10
)

# validación cruzada para tomek
results <- cross_validation(
  tomekFolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      tomek_y~.,
      data = train_data
    )
    
    list(model_h1n1)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    auc(roc(h1n1_predicition[, 2], test_data$tomek_y))
  }
)

tomekNaiveBayesModel <- naiveBayes(tomek_x, tomek_y)

test1 <- predict(tomekNaiveBayesModel, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_seasonal_and_tomeklink_for_h1n1.csv"
)


# Undersampling de la clase mayoritaria con seleccion de caracteristicas

results <- cross_validation(
  tomekFolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      tomek_y~.,
      data = train_data %>% select(
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
        tomek_y
      )
    )
    
    list(model_h1n1)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    auc(roc(h1n1_predicition[, 2], test_data$tomek_y))
  }
)

tomekNaiveBayesModel <- naiveBayes(
  tomek_x %>% select(
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
  tomek_y
)

test1 <- predict(tomekNaiveBayesModel, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_h1n1_and_seasonal_and_tomeklink_for_h1n1.csv"
)




# backtracking para seleccionar features para h1n1

dataFrame <- read_dataset(
  "src/data/train_imputed.csv"
)


evaluator_backtracking <- function(attributes, data) {
  model <- naiveBayes(
    FSelectorRcpp::to_formula(attributes, "h1n1_vaccine"),
    data = data
  )
  
  x <- data %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
  probs <- predict(model, newdata = x, type="raw")
  
  auc(roc(probs[, 2], data$h1n1_vaccine))
}


best_result <- 0
best_nodes <- c()


backtracking <- function (
  candidates, node_result, lower_bound, dataFrame
) {
  if (lower_bound > length(candidates)) {
    return(lower_bound + 1)
  }
  
  index <- lower_bound
  
  useless_ <- sapply(
    candidates[index:length(candidates)],
    function (x) {
      message(paste("Before", paste(node_result, collapse = ",")))
      
      if (!(x %in% node_result)) {
        node_result <- c(node_result, x)
      }
      
      message(paste("After", paste(node_result, collapse = ",")))
      
      result <- evaluator_backtracking(node_result, dataFrame)
      
      if (result < best_result) {
        node_result <- node_result[-length(node_result)]
        
        if (lower_bound == index) {
          return(lower_bound + 1)
        }
        
        index <- index + 1
      } else {
        best_result <<- result
        best_nodes <<- node_result
        index <- backtracking(
          candidates,
          node_result,
          index + 1,
          dataFrame
        )
      }
      
      return(lower_bound + 1)
    }
  )
}

backtracking(
  names(dataFrame)[1:(length(names(dataFrame))-2)],
  c(),
  1,
  dataFrame
)


best_nodes
# h1n1_concern,h1n1_knowledge,behavioral_antiviral_meds,behavioral_face_mask,behavioral_wash_hands,doctor_recc_h1n1,chronic_med_condition,child_under_6_months,health_worker,health_insurance,opinion_h1n1_vacc_effective,opinion_h1n1_risk,age_group,race,hhs_geo_region,census_msa,household_adults,household_children,employment_occupation

results <- cross_validation(
  kfolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      h1n1_vaccine~.,
      data = train_data %>% select(
        h1n1_concern,
        h1n1_knowledge,
        behavioral_antiviral_meds,
        behavioral_face_mask,
        behavioral_wash_hands,
        doctor_recc_h1n1,
        chronic_med_condition,
        child_under_6_months,
        health_worker,
        health_insurance,
        opinion_h1n1_vacc_effective,
        opinion_h1n1_risk,
        age_group,
        race,
        hhs_geo_region,
        census_msa,
        household_adults,
        household_children,
        employment_occupation,
        h1n1_vaccine
      )
    )
    
    model_seasonal <- naiveBayes(
      seasonal_vaccine~.,
      data = train_data %>% select(
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
    )
    
    list(model_h1n1, model_seasonal)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    seasonal_predicition <- predict(
      models[[2]],
      newdata = test_data,
      type = "raw"
    )
    
    auc_h1n1 <- auc(roc(h1n1_predicition[, 2], test_data$h1n1_vaccine))
    auc_seasonal <- auc(roc(seasonal_predicition[, 2], test_data$seasonal_vaccine))
    
    mean(c(auc_h1n1, auc_seasonal))
  }
)


x_backtracking <- x %>% select(
  h1n1_concern,
  h1n1_knowledge,
  behavioral_antiviral_meds,
  behavioral_face_mask,
  behavioral_wash_hands,
  doctor_recc_h1n1,
  chronic_med_condition,
  child_under_6_months,
  health_worker,
  health_insurance,
  opinion_h1n1_vacc_effective,
  opinion_h1n1_risk,
  age_group,
  race,
  hhs_geo_region,
  census_msa,
  household_adults,
  household_children,
  employment_occupation
)

backtrackingNaiveBayesModel <- naiveBayes(x_backtracking, df$h1n1_vaccine)

test1 <- predict(backtrackingNaiveBayesModel, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_backtracking_for_h1n1_and_feature_selection_seasonal.csv"
)


# Undersampling de la clase mayoritaria con seleccion de caracteristicas una vez hecho el undersampling

undersampling_dataset <- cbind(tomek_x, tomek_y)
undersampling_dataset <- undersampling_dataset %>% rename(h1n1_vaccine=tomek_y)

evaluator_bayes_undersampling <- function(attributes, data, dependent = "h1n1_vaccine") {
  model <- naiveBayes(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x <- data %>% select(-h1n1_vaccine) %>% as.data.frame(.)
  probs <- predict(model, newdata = x, type="raw")
  
  auc(roc(probs[, 2], undersampling_dataset$h1n1_vaccine))
}

result_undersampling <- FSelectorRcpp::feature_search(
  attributes = names(undersampling_dataset)[1:35],
  fun = evaluator_bayes_undersampling,
  data = undersampling_dataset,
  parallel = T,
)

result_undersampling$best

results <- cross_validation(
  tomekFolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      tomek_y~.,
      data = train_data %>% select(
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
        tomek_y
      )
    )
    
    list(model_h1n1)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    auc(roc(h1n1_predicition[, 2], test_data$tomek_y))
  }
)

tomekNaiveBayesModel <- naiveBayes(
  h1n1_vaccine~.,
  data = undersampling_dataset %>% select(
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
)

test1 <- predict(tomekNaiveBayesModel, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_h1n1_and_seasonal_and_tomeklink_for_h1n1.csv"
)

# Undersampling de la clase mayoritaria con seleccion de caracteristicas (backward) una vez hecho el undersampling

evaluator_bayes_undersampling <- function(attributes, data, dependent = "h1n1_vaccine") {
  model <- naiveBayes(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x <- data %>% select(-h1n1_vaccine) %>% as.data.frame(.)
  probs <- predict(model, newdata = x, type="raw")
  
  auc(roc(probs[, 2], undersampling_dataset$h1n1_vaccine))
}

result_undersampling <- FSelectorRcpp::feature_search(
  attributes = names(undersampling_dataset)[1:35],
  fun = evaluator_bayes_undersampling,
  data = undersampling_dataset,
  parallel = T,
  type = "backward"
)

result_undersampling$best

results <- cross_validation(
  tomekFolds,
  function (train_data) {
    model_h1n1 <- naiveBayes(
      tomek_y~.,
      data = train_data %>% select(
        behavioral_antiviral_meds,
        doctor_recc_h1n1,
        health_worker,
        health_insurance,
        opinion_h1n1_vacc_effective,
        opinion_h1n1_risk,
        age_group,
        race,
        hhs_geo_region,
        census_msa,
        household_adults,
        household_children,
        employment_occupation,
        tomek_y
      )
    )
    
    list(model_h1n1)
  },
  function (models, test_data) {
    h1n1_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    auc(roc(h1n1_predicition[, 2], test_data$tomek_y))
  }
)

tomekNaiveBayesModelandbackwardselection <- naiveBayes(
  h1n1_vaccine~.,
  data = undersampling_dataset %>% select(
    behavioral_antiviral_meds,
    doctor_recc_h1n1,
    health_worker,
    health_insurance,
    opinion_h1n1_vacc_effective,
    opinion_h1n1_risk,
    age_group,
    race,
    hhs_geo_region,
    census_msa,
    household_adults,
    household_children,
    employment_occupation,
    h1n1_vaccine
  )
)

test1 <- predict(tomekNaiveBayesModelandbackwardselection, newdata = dftest, type="raw")
test2 <- predict(probablyBestModelseasonal, newdata = dftest, type="raw")

submission <- get_submission_dataframe(
  test1[, 2],
  test2[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_feature_selection_for_h1n1_backward_and_seasonal_and_tomeklink_for_h1n1.csv"
)



# Entrenamiento y prediccion con un solo modelo

df_one_target <- collapse_labels(df, 36, 37)
df_one_target$target = factor(df_one_target$target)
str(df_one_target$target)

oneTargetFolds <- get_kfolds(
  df_one_target,
  10
)

## training
results <- cross_validation(
  oneTargetFolds,
  function (train_data) {
    one_target_model <- naiveBayes(
      target~.-h1n1_vaccine-seasonal_vaccine,
      data = train_data
    )
    
    list(one_target_model)
  },
  function (models, test_data) {
    one_target_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    one_target_predicition_processed <- cbind(
      one_target_predicition[,3] + one_target_predicition[,4],
      one_target_predicition[,2] + one_target_predicition[,4]
    )
    
    h1n1_auc <- auc(roc(one_target_predicition_processed[,1], test_data$h1n1_vaccine))
    seasonal_auc <- auc(roc(one_target_predicition_processed[,2], test_data$seasonal_vaccine))
    
    mean(c(h1n1_auc, seasonal_auc))
  }
)

oneClassifierModel <- naiveBayes(target~.-h1n1_vaccine-seasonal_vaccine, data=df_one_target)

## test

oneClassifierModelpredTest <- predict(oneClassifierModel, newdata = dftest, type="raw")
oneClassifierModelprocessedPredTest <- cbind(
  oneClassifierModelpredTest[,3] + oneClassifierModelpredTest[,4],
  oneClassifierModelpredTest[,2] + oneClassifierModelpredTest[,4]
)

submission <- get_submission_dataframe(
  oneClassifierModelprocessedPredTest[, 1],
  oneClassifierModelprocessedPredTest[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_one_model.csv"
)

# Busqueda de mejores atributos cuando solo tenemos una etiqueta type forward

evaluator_bayes_one_target <- function(attributes, data, dependent = "target") {
  model <- naiveBayes(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  probs <- predict(model, newdata = data, type="raw")
  probsProcessed <- cbind(
    probs[,3] + probs[,4],
    probs[,2] + probs[,4]
  )
  
  mean(
    c(
      auc(roc(probsProcessed[, 1], data$h1n1_vaccine)),
      auc(roc(probsProcessed[, 2], data$seasonal_vaccine))
    )
  )
}

one_target_result <- FSelectorRcpp::feature_search(
  attributes = names(df_one_target)[1:35],
  fun = evaluator_bayes_one_target,
  data = df_one_target,
  parallel = T
)

# mejores atributos forward:
# h1n1_knowledge,doctor_recc_h1n1,doctor_recc_seasonal,chronic_med_condition,health_worker,health_insurance,opinion_h1n1_vacc_effective,opinion_h1n1_risk,opinion_seas_vacc_effective,opinion_seas_risk,opinion_seas_sick_from_vacc,age_group,education,race,rent_or_own,hhs_geo_region,employment_industry


## train

results <- cross_validation(
  oneTargetFolds,
  function (train_data) {
    one_target_model <- naiveBayes(
      target~.-h1n1_vaccine-seasonal_vaccine,
      data = train_data %>% select(
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
        employment_industry,
        h1n1_vaccine,
        seasonal_vaccine,
        target
      )
    )
    
    list(one_target_model)
  },
  function (models, test_data) {
    one_target_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )
    
    one_target_predicition_processed <- cbind(
      one_target_predicition[,3] + one_target_predicition[,4],
      one_target_predicition[,2] + one_target_predicition[,4]
    )
    
    h1n1_auc <- auc(roc(one_target_predicition_processed[,1], test_data$h1n1_vaccine))
    seasonal_auc <- auc(roc(one_target_predicition_processed[,2], test_data$seasonal_vaccine))
    
    mean(c(h1n1_auc, seasonal_auc))
  }
)

oneClassifierForwardFeatureSelectionModel <- naiveBayes(
  target~.-h1n1_vaccine-seasonal_vaccine,
  data = df_one_target %>% select(
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
    employment_industry,
    h1n1_vaccine,
    seasonal_vaccine,
    target
  )
)

## test

oneClassifierForwardFeatureSelectionModel.train.pred.test <- predict(
  oneClassifierForwardFeatureSelectionModel,
  newdata = dftest,
  type = "raw"
)

oneClassifierForwardFeatureSelectionModel.train.pred.test.processed <- cbind(
  oneClassifierForwardFeatureSelectionModel.train.pred.test[,3] + oneClassifierForwardFeatureSelectionModel.train.pred.test[,4],
  oneClassifierForwardFeatureSelectionModel.train.pred.test[,2] + oneClassifierForwardFeatureSelectionModel.train.pred.test[,4]
)

submission <- get_submission_dataframe(
  oneClassifierForwardFeatureSelectionModel.train.pred.test.processed[, 1],
  oneClassifierForwardFeatureSelectionModel.train.pred.test.processed[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_one_model_feature_selection_forward.csv"
)


# Busqueda de mejores atributos cuando solo tenemos una etiqueta type backward

one_target_result_backward <- FSelectorRcpp::feature_search(
  attributes = names(df_one_target)[1:35],
  fun = evaluator_bayes_one_target,
  data = df_one_target,
  parallel = T,
  type = "backward"
)

# mejores atributos backward:
# h1n1_knowledge,doctor_recc_h1n1,doctor_recc_seasonal,chronic_med_condition,health_worker,health_insurance,opinion_h1n1_vacc_effective,opinion_h1n1_risk,opinion_seas_vacc_effective,opinion_seas_risk,opinion_seas_sick_from_vacc,age_group,education,race,rent_or_own,hhs_geo_region,employment_industry

## train

results <- cross_validation(
  oneTargetFolds,
  function (train_data) {
    one_target_model <- naiveBayes(
      target~.-h1n1_vaccine-seasonal_vaccine,
      data = train_data %>% select(
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
        employment_industry,
        h1n1_vaccine,
        seasonal_vaccine,
        target
      )
    )

    list(one_target_model)
  },
  function (models, test_data) {
    one_target_predicition <- predict(
      models[[1]],
      newdata = test_data,
      type = "raw"
    )

    one_target_predicition_processed <- cbind(
      one_target_predicition[,3] + one_target_predicition[,4],
      one_target_predicition[,2] + one_target_predicition[,4]
    )

    h1n1_auc <- auc(roc(one_target_predicition_processed[,1], test_data$h1n1_vaccine))
    seasonal_auc <- auc(roc(one_target_predicition_processed[,2], test_data$seasonal_vaccine))

    mean(c(h1n1_auc, seasonal_auc))
  }
)

oneClassifierBackwardFeatureSelectionModel <- naiveBayes(
  target~.-h1n1_vaccine-seasonal_vaccine,
  data = df_one_target %>% select(
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
    employment_industry,
    h1n1_vaccine,
    seasonal_vaccine,
    target
  )
)

## test

oneClassifierBackwardFeatureSelectionModel.train.pred.test <- predict(
  oneClassifierBackwardFeatureSelectionModel,
  newdata = dftest,
  type = "raw"
)

oneClassifierBackwardFeatureSelectionModel.train.pred.test.processed <- cbind(
  oneClassifierBackwardFeatureSelectionModel.train.pred.test[,3] + oneClassifierBackwardFeatureSelectionModel.train.pred.test[,4],
  oneClassifierBackwardFeatureSelectionModel.train.pred.test[,2] + oneClassifierBackwardFeatureSelectionModel.train.pred.test[,4]
)

submission <- get_submission_dataframe(
  oneClassifierBackwardFeatureSelectionModel.train.pred.test.processed[, 1],
  oneClassifierBackwardFeatureSelectionModel.train.pred.test.processed[, 2]
)

submission %>% write_csv(
  "src/BAYES/results/submission_bayes_nomice_but_imputed_train_nomice_but_imputed_test_one_model_feature_selection_forward.csv"
)


###########################################################3


df_all_imputed <- read_dataset("src/data/train_all_imputed.csv")
dftest_all_imputed <- read_dataset("src/data/test_all_imputed.csv")


# Recuperación del resto de valores perdidos con MICE (10 iteraciones) solo para train

train_all_imputed_naive_bayes_model_h1n1 <- naiveBayes(
  h1n1_vaccine~.,
  data = df_all_imputed
)

train_all_imputed_naive_bayes_model_h1n1_pred <- predict(
  train_all_imputed_naive_bayes_model_h1n1,
  newdata = dftest,
  type = "raw"
)

train_all_imputed_naive_bayes_model_seasonal <- naiveBayes(
  seasonal_vaccine~.,
  data = df_all_imputed
)

train_all_imputed_naive_bayes_model_seasonal_pred <- predict(
  train_all_imputed_naive_bayes_model_seasonal,
  newdata = dftest,
  type = "raw"
)

submission <- get_submission_dataframe(
  train_all_imputed_naive_bayes_model_h1n1_pred[, 2],
  train_all_imputed_naive_bayes_model_seasonal_pred[, 2]
)

submission %>% write_csv("src/BAYES/results/submission_bayes_train_imputed_mice_and_original_test.csv")


#################
## Recuperación del resto de valores perdidos con MICE (10 iteraciones) solo para train y test

train_and_test_all_imputed_naive_bayes_model_h1n1 <- naiveBayes(
  h1n1_vaccine~.,
  data = df_all_imputed
)

train_and_test_all_imputed_naive_bayes_model_h1n1_pred <- predict(
  train_and_test_all_imputed_naive_bayes_model_h1n1,
  newdata = dftest_all_imputed,
  type = "raw"
)

train_and_test_all_imputed_naive_bayes_model_seasonal <- naiveBayes(
  seasonal_vaccine~.,
  data = df_all_imputed
)

train_and_test_all_imputed_naive_bayes_model_seasonal_pred <- predict(
  train_and_test_all_imputed_naive_bayes_model_seasonal,
  newdata = dftest_all_imputed,
  type = "raw"
)

submission <- get_submission_dataframe(
  train_and_test_all_imputed_naive_bayes_model_h1n1_pred[, 2],
  train_and_test_all_imputed_naive_bayes_model_seasonal_pred[, 2]
)

submission %>% write_csv("src/BAYES/results/submission_bayes_train_imputed_mice_and_test_imputed_mice.csv")


#################
## Recuperación del resto de valores perdidos con MICE (10 iteraciones) solo para train y con los mejores atributos

train_all_imputed_naive_bayes_model_h1n1_feature_selection <- naiveBayes(
  h1n1_vaccine~.,
  data = df_all_imputed %>% select(
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
)

train_all_imputed_naive_bayes_model_h1n1_feature_selection_pred <- predict(
  train_all_imputed_naive_bayes_model_h1n1_feature_selection,
  newdata = dftest,
  type = "raw"
)

train_all_imputed_naive_bayes_model_seasonal_feature_selection <- naiveBayes(
  seasonal_vaccine~.,
  data = df_all_imputed %>% select(
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
)

train_all_imputed_naive_bayes_model_seasonal_feature_selection_pred <- predict(
  train_all_imputed_naive_bayes_model_seasonal_feature_selection,
  newdata = dftest,
  type = "raw"
)

submission <- get_submission_dataframe(
  train_all_imputed_naive_bayes_model_h1n1_feature_selection_pred[, 2],
  train_all_imputed_naive_bayes_model_seasonal_feature_selection_pred[, 2]
)

submission %>% write_csv("src/BAYES/results/submission_bayes_train_imputed_mice_and_test_imputed_feature_selection_for_both.csv")

