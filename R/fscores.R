#' f-score 
#'
#' A simple function to generate F-scores (Fisher scores) for ranking features
#' 
##'@param Data        (dataframe) Data dataframe
#' @param featSel     (numeric) column with different classes
#' @param featureCol  (numeric) all the columns that contain features
#' @return named numeric \code{f-scores} 
#'
#'@author
#'Atesh Koul, C'MON group, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
fscore <- function(Data,featSep,featureCol){
  tr <- function(m) return(sum(diag(m)))
  # separate positive and negative feature sets
  posIns  <- Data[Data[,featSep]==unique(Data[,featSep])[1],]
  negIns  <- Data[Data[,featSep]==unique(Data[,featSep])[2],]
  f_score <- vector()
  featureColNames <- names(Data)[featureCol]

  for(feature in featureColNames){
    f_score[feature] = (norm(as.matrix(mean(posIns[,feature])-mean(negIns[,feature])))^2)/(tr(cov(as.matrix(posIns[,feature]))) + tr(cov(as.matrix(negIns[,feature]))))
  }
  return(f_score)
}
