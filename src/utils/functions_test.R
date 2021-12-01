source("src/utils/functions.R")



### COLLAPSING LABELS

dataframe = data.frame(
  x  = c(0,0,0,0),
  y1 = c(0,0,1,1),
  y2 = c(0,1,0,1)
)

col_index_1 = 2
col_index_2 = 3

collapse_labels(dataframe, col_index_1, col_index_2)



### SPLITTING LABELS


probs_dataframe = data.frame(
  id  = c(  1,   2,   3),
  p00 = c(0.1, 0.2, 0.3),
  p01 = c(0.4, 0.5, 0.6),
  p10 = c(0.7, 0.8, 0.9),
  p11 = c(0.1, 0.2, 0.3)
)

split_target(probs_dataframe)
