fun.auc <- function(pred,obs){
  
  # Run the ROCR functions for AUC calculation
  ROC_perf <- performance(prediction(pred,obs),"tpr","fpr")
  ROC_sens <- performance(prediction(pred,obs),"sens","spec")
  ROC_err <- performance(prediction(pred, labels=obs),"err")
  ROC_auc <- performance(prediction(pred,obs),"auc")
  
  # AUC value
  AUC <- ROC_auc@y.values[[1]] # AUC
  
  # Mean sensitivity across all cutoffs
  x.Sens <- mean(as.data.frame(ROC_sens@y.values)[,1])
  
  # Mean specificity across all cutoffs
  x.Spec <- mean(as.data.frame(ROC_sens@x.values)[,1])
  
  # Sens-Spec table to estimate threshold cutoffs
  SS <- data.frame(SENS=as.data.frame(ROC_sens@y.values)[,1],SPEC=as.data.frame(ROC_sens@x.values)[,1])
  
  # Threshold cutoff with min difference between Sens and Spec
  SS_min_dif <- ROC_perf@alpha.values[[1]][which.min(abs(SS$SENS-SS$SPEC))]
  
  # Threshold cutoff with max sum of Sens and Spec
  SS_max_sum <- ROC_perf@alpha.values[[1]][which.max(rowSums(SS[c("SENS","SPEC")]))]
  
  # Min error rate
  Min_Err <- min(ROC_err@y.values[[1]])
  
  # Threshold cutoff resulting in min error rate
  Min_Err_Cut <- ROC_err@x.values[[1]][which(ROC_err@y.values[[1]]==Min_Err)][1]
  
  # Kick out the values
  round(cbind(AUC,x.Sens,x.Spec,SS_min_dif,SS_max_sum,Min_Err,Min_Err_Cut),3)
}