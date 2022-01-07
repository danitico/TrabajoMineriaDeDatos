source("src/utils/functions.R")
source("src/knn/knn_functions.R")
source("src/knn/knn_distances.R")

set.seed(0)



### CARGA DE LOS DATOS

df <- read_dataset("src/data/train_imputed.csv")

h1n1_label <- df$h1n1_vaccine
seasonal_label <- df$seasonal_vaccine
df <- collapse_labels(df, 36, 37)



### PREPARACIÓN DE LOS DATOS

df_sample <- sample(1:nrow(df), size=500)
experimental_df <- df[df_sample,]

n <- nrow(experimental_df)
training_proportion <- 0.8
sample_size <- floor(training_proportion * n)
train_positions <- sample(1:n, size=sample_size)

experimental_df_train <- experimental_df[train_positions,]
experimental_df_test <- experimental_df[-train_positions,]

last_experimental_col_index <- ncol(experimental_df_train)



### APLICACIÓN DEL CLASIFICADOR

experimental_predictions_df <- knn_classifier(
  experimental_df_train[,-last_experimental_col_index] %>% select_if(function(x) is.ordered(x)),
  experimental_df_train[, last_experimental_col_index],
  experimental_df_test[, -last_experimental_col_index] %>% select_if(function(x) is.ordered(x)),
  k=7,
  get_hamming_distance(T)
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

df_train <- select_if(df, function(x) is.ordered(x))

row_sample = sample(1:nrow(df_train), 50)

df_train_sampled = df_train[row_sample,]
df_label_sampled = df[row_sample, "target"]

last_real_col_index = ncol(df_train_sampled)

predictions_df <- knn_classifier(
  df_train_sampled[,-last_real_col_index],
  df_label_sampled,
  df_test,
  k=7,
  get_hamming_distance(T),
  T
)

submission_df = predictions_df %>%
  add_real_respondent_id() %>% 
  split_target()

write_csv(
  submission_df,
  "src/knn/results/sample_50_k_7_hamming_generalised.csv"
)
