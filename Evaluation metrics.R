getEvaluationMetrics <- function(cm) {
  
  TP <- cm[2,2] # true positive
  TN <- cm[1,1] # true negative
  FP <- cm[1,2] # false positive
  FN <- cm[2,1] # false negative
  
  accuracy = sum(diag(cm)) / sum(cm)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  c(Accuracy = accuracy, 
    Precision = precision, 
    Recall = recall, 
    F1 = F1)
}
