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
