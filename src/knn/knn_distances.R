library(magrittr)


# Auxiliar
base_hamming_distance <- function(x1, x2) {
  as.integer(x1 != x2)
}


# Auxiliar
generalised_hamming_distance <- function(x1, x2) {
  ifelse(
    is.ordered(x1), # Se asume que x2 es de la misma clase
    abs(as.integer(x1) - as.integer(x2)) / (length(levels(x1)) - 1), # Se divide para escalar la distancia máxima a 1
    as.integer(x1 != x2)
  )
}



hamming_distance <- function(x, y, is_generalised) {
  
  component_distance <- function(x1, x2) {
    ifelse(
      is_generalised,
      generalised_hamming_distance(x1, x2),
      base_hamming_distance(x1, x2)
    ) %>% ifelse(is.na(.), 1, .)
  }

  sapply(
    1:length(ncol(x)),
    function(i) component_distance(x[[i]], y[[i]])
  ) %>% sum()
}


get_hamming_distance <- function(is_generalised) {
  function(x, y) hamming_distance(x, y, is_generalised)
}



gower_distance <- function(x, y) {
  
  component_distance <- function(x1, x2) as.integer(x1 == x2)
  
  numerator <- sapply(
    1:length(ncol(x)),
    function(i) component_distance(x[[i]], y[[i]])
  )
  
  1 - sum(numerator, na.rm = T) / sum(is.na(numerator))
}



jaccard_distance <- function(x,y) {
  
  n <- ncol(x)
  
  index_row <- function(row) sapply(
    1:n,
    function(i) paste(i,as.integer(row[[i]]),sep="_")
  )
  
  x_indexed <- index_row(x)
  y_indexed <- index_row(y)
  
  intersection_cardinal <- sapply(
    1:n,
    function(i) x_indexed[i] == y_indexed[i]
  ) %>% sum()
  union_cardinal <- length(x_indexed) + length(y_indexed) - intersection_cardinal
  
  # El índice de Jaccard mide la similaridad entre dos conjuntos. Aquí se calcula
  # uno menos el índice, que da la distancia (a mayor similaridad, menor distancia).
  1 - intersection_cardinal / union_cardinal
}
