TPR <- function(where_pos){
  
  TP <- sum(where_pos)
  return(list(TP/length(where_pos), TP))
  
}

FPR <- function(where_neg){
  
  FP <- sum(where_neg)
  return(list(FP/length(where_neg), FP))
  
}

FNR <- function(where_pos){
  
  FN <- sum(where_pos == 0)
  return(list(FN/length(where_pos), FN))
  
}

TNR <- function(where_neg){
  
  TN <- sum(where_neg == 0)
  return(list(TN/length(where_neg), TN))
  
}