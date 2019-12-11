# --------------------------------------------------------------------------------------------------
# Standard: 
# ===
# --------------------------------------------------------------------------------------------------
#' 
#' @param df working data.frame 
#'
#' @return List with:
#'  \(df): input data.frame with changes for the numeric columns
# --------------------------------------------------------------------------------------------------

regularize <- function(df,Categorias,dfTest){
  
  #Continuous variables estandarization
  VarFac<-as.character(df$VARIABLE)
  
  if (length(VarFac)>0){
    for(variable in VarFac){
      media <- mean(df[,variable],na.rm = T)
      st_dev <- sd(df[,variable],na.rm = T)
      df[,variable]<-(df[,variable]-media)/st_dev
    }
  }
  return(df)
}


