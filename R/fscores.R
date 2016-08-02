#' f-score 
#'
#' A simple function to generate F-scores (Fisher scores) for ranking features
#' 
##'@param Data        (dataframe) Data dataframe
#' @param featSel     (numeric) column with different classes
#' @param featureCol  (numeric) all the columns that contain features
#' @return named numeric \code{f-scores} 
#' @examples 
#' # calculate f-scores for 10% of movement
#' fscore(KinData,featSep = 1,featureCol = c(2,12,22,32,42,52,62,72,82,92,102,112))
#'
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
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
