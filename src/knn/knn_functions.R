library(magrittr)

source("src/knn/knn_distances.R")



get_probs_uniform <- function(v) {
  probs_of_class <- function(class) {
    mean(class == v)
  }
  sapply(
    c("00", "01", "10", "11"),
    probs_of_class
  )
}



get_probs_weighted <- function(v) {
  k <- length(v)
  probs_of_class <- function(class) {
    sapply(
      1:k,
      function(i) (k+1-i)*(class==v[i])
    ) / (k*(k+1))/2
    # Nota: (k*(k+1))/2 == 1+2+...+k``
  }
  sapply(
    c("00", "01", "10", "11"),
    probs_of_class
  )
}



predict_class_for_given <- function(x0, train_features, train_labels, n_test, k, dist_func, get_probs_func, print_progress) {
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
  get_probs_func(train_labels[neighbourhood_positions,1])
}



knn_classifier <- function(train_features, train_labels, test_features, k=1, dist_func, get_probs_func, print_progress=F) {
  test_features <- cbind(1:nrow(test_features), test_features)
  predictions <- sapply(1:nrow(test_features), function(i) {
    x0 <- test_features[i,]
    predict_class_for_given(x0, train_features, train_labels, nrow(test_features), k, dist_func, get_probs_func, print_progress)
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



predict_class_for_given_bin <- function(x0, train_features, train_labels, n_test, k, dist_func, print_progress) {
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
  mean(
    as.integer(train_labels[neighbourhood_positions,1])-1,
    na.rm=T
  )
}



knn_classifier_bin <- function(train_features, train_labels, test_features, k=1, dist_func, print_progress=F) {
  test_features <- cbind(1:nrow(test_features), test_features)
  predictions <- sapply(1:nrow(test_features), function(i) {
    x0 <- test_features[i,]
    predict_class_for_given_bin(x0, train_features, train_labels, nrow(test_features), k, dist_func, print_progress)
  })
  predictions
}
