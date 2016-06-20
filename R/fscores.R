#'simple function to generate F-scores (Fisher scores) for ranking features
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item Data = Data dataframe
#'      \item featSel = column with different classes
#'      \item featureCol = all the columns that contain features
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item named numeric f-scores
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
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
