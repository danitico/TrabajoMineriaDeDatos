library(tidyverse)
library(AUC)



### DE MULTIETIQUETA A MULTICLASE

collapse_labels <- function(dataframe, col_index_1, col_index_2) {
  
  new_dataframe <- dataframe[,-c(col_index_1, col_index_2)]
  
  new_dataframe$target <- paste(
    dataframe[[colnames(dataframe)[col_index_1]]],
    dataframe[[colnames(dataframe)[col_index_2]]],
    sep=""
  ) %>% as.factor()
  return(new_dataframe)
}



### SEPARACIÓN DE PROBABILIDADES

split_target <- function(probs_dataframe) {
  # Se asume que probs_dataframe tiene una fila por instancia y es de la forma (id, prob(0,0), prob(0,1), prob(1,0), prob(1,1))
  
  new_dataframe <- apply(probs_dataframe, 1, function(row) c(
      row[1], # id
      row[4]+row[5], # p(10)+p(11)
      row[3]+row[5]  # p(01)+(11)
    )) %>% t()
    
  colnames(new_dataframe) = c("respondent_id", "h1n1_vaccine", "seasonal_vaccine")
  return(as.data.frame(new_dataframe))
}



### PUNTUACIÓN SEGÚN LA COMPETICIÓN

get_score <- function(predictions_df, h1n1_labels, seasonal_labels) {
  # Se asume que predictions_df tiene el formato que se requiere para la entrega
  mean(
    auc(roc(predictions_df$h1n1_vaccine, as.factor(h1n1_labels))),
    auc(roc(predictions_df$seasonal_vaccine, as.factor(seasonal_labels)))
  )
}



### UNIFICACIÓN DE LOS DATA FRAMES

get_unified_training_dataset <- function () {
  feature <- read_csv("src/data/drivendata/training_set_features.csv")
  labels <- read_csv("src/data/drivendata/training_set_labels.csv")
  
  # Unión de dataframes
  df <- merge(
    feature,
    labels,
    by = "respondent_id"
  )

  # Eliminación de la variable id
  df$respondent_id <- NULL
  
  df %>% write_csv("src/data/drivendata/train.csv")
}


get_test_dataset <- function () {
  df <- read_csv("src/data/drivendata/test_set_features.csv")
  
  df$respondent_id <- NULL
  
  df %>% write_csv("src/data/drivendata/test.csv")
}


add_real_respondent_id <- function(dataframe) {
  respondent_id <- 26707:53414
  cbind(
    respondent_id,
    dataframe
  ) %>% as.data.frame()
}



### LECTURA DEL DATASET CON TODAS LAS COLUMNAS COMO FACTORES

read_dataset <- function(file) {
  
  read_csv(file) %>%
    mutate(
      across(
        everything(),
        ~ factor(.x)
        )
      ) %>%
    mutate(
      across(
        c(
          h1n1_concern,
          h1n1_knowledge,
          opinion_h1n1_vacc_effective, 
          opinion_h1n1_risk,
          opinion_h1n1_sick_from_vacc,
          opinion_seas_vacc_effective,
          opinion_seas_risk,
          opinion_seas_sick_from_vacc,
          age_group,
          education,
          income_poverty,
          household_adults,
          household_children
          ),
        ~ as.ordered(.x)
        )
      )
}



### DATAFRAME DEL FORMATO DE ENTREGA

get_submission_dataframe <- function(h1n1_vaccine_probs, seasonal_vaccine_probs) {
  submission <- read_csv("src/data/drivendata/submission_format.csv")
  
  submission$h1n1_vaccine <- h1n1_vaccine_probs
  submission$seasonal_vaccine <- seasonal_vaccine_probs
  
  submission
}



### ENSEMBLE

bagging_ensemble_classifier <- function(submission_df_list, ponderation_vector) {
  
  denominator <- sum(ponderation_vector)
  number_of_instances <- 26708
  
  # Para aplicar bagging a la columna h1n1 o a la de seasonal
  bagging_column <- function(column_number) {
    
    # Dado un row_number, vector de cinco posiciones que tiene en la posición
    # i-ésima la probabilidad que el i-ésimo clasificador ha calculado de que
    # la variable en column_number valga 1
    probs_of_row <- function(row_number) {
      sapply(
        submission_df_list,
        function(classification_df) classification_df[row_number, column_number]
      ) %>% unlist()
    }
    
    sapply(
      1:number_of_instances,
      function(row_number) sum(ponderation_vector*probs_of_row(row_number)) / denominator
    )
  }
  
  data.frame(
    respondent_id = 26707:53414,
    h1n1_vaccine = bagging_column(2),
    seasonal_vaccine = bagging_column(3)
  )
  
}
