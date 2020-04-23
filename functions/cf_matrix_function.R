# Function for calculating confusion matrix summary statistics
cf_matrix <- function(actual_vec, pred_prob_vec, positive_val, 
                      cut_prob = 0.5, search_cut = FALSE) {
  
  if (search_cut == FALSE) {
    actual <- actual_vec == positive_val; pred <- pred_prob_vec >= cut_prob
    P <- sum(actual); N <- length(actual) - P; TP <- sum(actual & pred)
    FN <- P - TP; TN <- sum(!(actual) & !(pred)); FP <- N - TN
    
    if (TP != 0) { Precision <- TP/(TP + FP); Recall <- TP/(TP + FN)
    F1 <- 2*((Precision*Recall)/(Precision + Recall))}
    
    if(TP == 0) { Precision = 0; Recall = 0; F1 = 0 }
    
    model_results <- list(confusion_matrix = 
                            data.frame(metric = c("Correct", "Misclassified", "True Positive",
                                                  "True Negative","False Negative", "False Positive"),
                                       observations = c(TN + TP, FN + FP, TP, TN, FN, FP),
                                       rate = c((TN + TP)/(N + P), (FN + FP)/(N + P), TP/P, TN/N, FN/P, FP/N),
                                       pct_total_obs = c((TN + TP), (FN + FP), TP, TN, FN, FP)*(1/(N + P)),
                                       stringsAsFactors = FALSE),
                          F1_summary = 
                            data.frame(metric = c("Precision", "Recall", "F1 Score"),
                                       value = c(Precision, Recall, F1),
                                       stringsAsFactors = FALSE))
    return(model_results) } 
  
  if (search_cut == TRUE) {
    optimal_cut = data.frame(cut_prob = seq(0,1, by = 0.05),
                             correct_rate = NA, F1_score = NA,
                             false_pos_rate = NA, false_neg_rate = NA)
    
    for (row in (1:nrow(optimal_cut))) {
      actual <- actual_vec == positive_val 
      pred <- pred_prob_vec >= optimal_cut$cut_prob[row]
      P <- sum(actual); N <- length(actual) - P
      TP <- sum(actual & pred); FN <- P - TP
      TN <- sum(!(actual) & !(pred)); FP <- N - TN
      
      if (TP != 0) { Precision <- TP/(TP + FP); Recall <- TP/(TP + FN)
      F1 <- 2*((Precision*Recall)/(Precision + Recall))}
      
      if(TP == 0) { Precision = 0; Recall = 0; F1 = 0 }
      
      optimal_cut[row, 2:5] <- c((TN + TP)/(N + P), F1, FP/N, FN/P)
    } 
    return(optimal_cut)
  }
}