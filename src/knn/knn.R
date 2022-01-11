source("src/utils/functions.R")
source("src/knn/knn_functions.R")
source("src/knn/knn_distances.R")

library(FSelectorRcpp)

set.seed(0)

training_proportion <- 0.8



### CARGA DE LOS DATOS

df <- read_dataset("src/data/train_imputed.csv")

h1n1_label <- df$h1n1_vaccine
seasonal_label <- df$seasonal_vaccine
df_collapsed <- collapse_labels(df, 36, 37)



### PREPARACIÓN DE LOS DATOS

df_sample <- sample(1:nrow(df_collapsed), size=500)
experimental_df <- df_collapsed[df_sample,]

n <- nrow(experimental_df)
training_proportion <- 0.8
sample_size <- floor(training_proportion * n)
train_positions <- sample(1:n, size=sample_size)

experimental_df_train <- experimental_df[train_positions,]
experimental_df_test <- experimental_df[-train_positions,]

last_experimental_col_index <- ncol(experimental_df_train)



### APLICACIÓN DEL CLASIFICADOR

experimental_predictions_df <- knn_classifier(
  experimental_df_train[,-last_experimental_col_index] %>% select_if(function(x) !is.ordered(x)),
  experimental_df_train[, last_experimental_col_index],
  experimental_df_test[, -last_experimental_col_index] %>% select_if(function(x) !is.ordered(x)),
  k=7,
  get_hamming_distance(T),
  get_probs_uniform
)



### EVALUCACIÓN DE LA CLASIFICACIÓN

experimental_submission_df = experimental_predictions_df %>%
  add_experimental_respondent_id() %>% 
  split_target()

get_score(
  experimental_submission_df,
  h1n1_label[df_sample][-train_positions],
  seasonal_label[df_sample][-train_positions]
)



### CLASIFICACIÓN CON EL CONJUNTO DE TEST DE LA ENTREGA

df_test <- read_dataset("src/data/test_imputed.csv") %>% select_if(function(x) is.ordered(x))

df_train <- select_if(df_collapsed, function(x) is.ordered(x))

row_sample = sample(1:nrow(df_train), 50)

df_train_sampled = df_train[row_sample,]
df_label_sampled = df_collapsed[row_sample, "target"]

last_real_col_index = ncol(df_train_sampled)

predictions_df <- knn_classifier(
  df_train_sampled[,-last_real_col_index],
  df_label_sampled,
  df_test,
  k=7,
  gower_distance,
  get_probs_uniform,
  T
)

submission_df = predictions_df %>%
  add_real_respondent_id() %>% 
  split_target()

write_csv(
  submission_df,
  "src/knn/results/sample_50_k_7_gower.csv"
)



### VARIABLES Y FUNCIONES AUXILIARES

classes_representants_func <- function(k) lapply( # Sobremuestreo
  c("00", "01", "10", "11"),
  function(class) {
    sample(
      which(df_collapsed$target == class),
      size=k
    )
  }
) %>% unlist()

classes_representants = classes_representants_func(k=50)
best_attributes <- cut_attrs(information_gain(target ~ ., df_collapsed), k=10)



### CLASIFICACIÓN CON MAYOR PREPROCESAMIENTO

experimental_df <- df_collapsed[classes_representants,]
n <- nrow(experimental_df)
sample_size <- floor(training_proportion * n)
train_positions <- sample(1:n, size=sample_size)

experimental_df_train <- experimental_df[train_positions, best_attributes]
experimental_df_test <- experimental_df[-train_positions, best_attributes]


experimental_predictions_df <- knn_classifier(
  experimental_df_train,
  experimental_df[, "target"],
  experimental_df_test,
  k=7,
  jaccard_distance,
  get_probs_uniform,
  T
)


experimental_submission_df = experimental_predictions_df %>%
  add_experimental_respondent_id() %>% 
  split_target()

get_score(
  experimental_submission_df,
  h1n1_label[classes_representants][-train_positions],
  seasonal_label[classes_representants][-train_positions]
)



### CLASIFICACIÓN CON EL CONJUNTO DE TEST DE LA ENTREGA CON MAYOR PREPROCESAMIENTO

df_train <- df_collapsed[classes_representants, best_attributes]
df_test <- read_dataset("src/data/test_imputed.csv")[, best_attributes]

predictions_df <- knn_classifier(
  df_train,
  df_collapsed[classes_representants, "target"],
  df_test,
  k=7,
  jaccard_distance,
  get_probs_uniform,
  T
)

submission_df = predictions_df %>%
  add_real_respondent_id() %>% 
  split_target()

write_csv(
  submission_df,
  "src/knn/results/best_10_preprocessed_50_k_7_jaccard_uniform.csv"
)



### CLASIFICACIÓN POR SEPARADO

best_attributes_h1n1 <- cut_attrs(information_gain(h1n1_vaccine ~ ., select(df, -seasonal_vaccine)), k=10)
best_attributes_seasonal <- cut_attrs(information_gain(seasonal_vaccine ~ ., select(df, -h1n1_vaccine)), k=10)
classes_representants_30 <- classes_representants_func(30)

df_separated_train_h1n1 <- df[classes_representants_30, best_attributes_h1n1]
df_separated_train_seasonal <- df[classes_representants_30, best_attributes_seasonal]

n <- length(classes_representants_30)
sample_size <- floor(training_proportion * n)
train_positions <- sample(1:n, size=sample_size)

experimental_predictions_df_h1n1 <- knn_classifier_bin(
  df_separated_train_h1n1[train_positions,],
  as.data.frame(h1n1_label[train_positions]),
  df_separated_train_h1n1[-train_positions,],
  k=7,
  jaccard_distance,
  T
)

experimental_predictions_df_seasonal <- knn_classifier_bin(
  df_separated_train_seasonal[train_positions,],
  as.data.frame(h1n1_label[train_positions]),
  df_separated_train_seasonal[-train_positions,],
  k=7,
  jaccard_distance,
  T
)

mean(
  auc(roc(experimental_predictions_df_h1n1, as.factor(h1n1_label[classes_representants][-train_positions]))),
  auc(roc(experimental_predictions_df_seasonal, as.factor(seasonal_label[classes_representants][-train_positions])))
)



### CLASIFICACIÓN CON EL CONJUNTO DE TEST DE LA ENTREGA POR SEPARADO

predictions_df_h1n1 <- knn_classifier_bin(
  df_separated_train_h1n1,
  as.data.frame(df$h1n1_vaccine[classes_representants_30]),
  read_dataset("src/data/test_imputed.csv")[, best_attributes_h1n1],
  k=7,
  jaccard_distance,
  T
)

predictions_df_seasonal <- knn_classifier_bin(
  df_separated_train_seasonal,
  as.data.frame(df$seasonal_vaccine[classes_representants_30]),
  read_dataset("src/data/test_imputed.csv")[, best_attributes_seasonal],
  k=7,
  jaccard_distance,
  T
)

write_csv(
  data.frame(
    respondent_id = 26707:53414,
    h1n1_vaccine = predictions_df_h1n1,
    seasonal_vaccine = predictions_df_seasonal
  ),
  "src/knn/results/best_10_preprocessed_30_k_7_jaccard_uniform_separated.csv"
)



### CLASIFICACIÓN ENSEMBLE

# Se van a usar como clasificadores débiles los clasificadores que han obtenido puntuaciones por encima de 0.7
# Las ponderaciones serán las proporcionales a la puntuación que han obtenido en Driven Data

bagging_ensemble_classifier(
  list(
    read_csv(
      "src/knn/results/best_10_preprocessed_50_k_7_jaccard_uniform.csv", 
      col_types = cols(respondent_id = col_integer())
      ),
    read_csv(
      "src/knn/results/best_10_preprocessed_30_k_7_jaccard_uniform_separated.csv", 
      col_types = cols(respondent_id = col_integer())
      )
    ),
  c(0.7653, 0.7175)
) %>%
  write_csv("src/knn/results/bagging.csv")
