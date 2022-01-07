library(magrittr)

source("src/knn/knn_distances.R")



get_probs <- function(v) {
  probs_of_class <- function(class) {
    mean(class == v)
  }
  sapply(
    c("00", "01", "10", "11"),
    probs_of_class
  )
}



predict_class_for_given <- function(x0, train_features, train_labels, n_test, k, dist_func, print_progress) {
  if (print_progress) {
    print(paste("Predicting instance #", x0[1], " of ", n_test, "."))
  }
  x0 <- x0[-1]
  distances <- sapply(1:nrow(train_features), function(i) {
    x <- train_features[i,]
    dist_func(x0,x)
    })
  neighbourhood_positions <- order(distances)[1:k]
  # Nota: train_features == test_features y k==1 lleva a cada observación a compararse consigo mismo
  get_probs(train_labels[neighbourhood_positions,1])
}



knn_classifier <- function(train_features, train_labels, test_features, k=1, dist_func, print_progress=F) {
  test_features <- cbind(1:nrow(test_features), test_features)
  predictions <- sapply(1:nrow(test_features), function(i) {
    x0 <- test_features[i,]
    predict_class_for_given(x0, train_features, train_labels, nrow(test_features), k, dist_func, print_progress)
    })
  as.data.frame(t(predictions))
}



add_experimental_respondent_id <- function(dataframe) {
  respondent_id <- 1:nrow(dataframe)
  cbind(
    respondent_id,
    dataframe
  ) %>% as.data.frame()
}
