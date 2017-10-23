#' f-score 
#'
#' A simple function to generate F-scores (Fisher scores) for ranking features
#' 
##'@param Data         (dataframe) Data dataframe
#' @param classCol     (numeric) column with different classes
#' @param featureCol   (numeric) all the columns that contain features
#' @param silent       (optional) (logical) whether to print messages or not
#' 
#' @details 
#' The function implements F-score for feature selection.
#' F-score provides a measure of how well a single feature at a time can discriminate between different 
#' classes. The higher the F-score, the better the discriminatory power of that feature
#' 
#' The F-score is calculated for two classes
#' 
#' 
#' @return named numeric \code{f-scores} 
#' @examples 
#' # calculate f-scores for 10% of movement
#' fscore(KinData,classCol = 1,featureCol = c(2,12,22,32,42,52,62,72,82,92,102,112))
#' # Output:
#' # Performing Feature selection f-score analysis 
#' # --f-scores--
#'
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#'
#'@references 
#'
#'Duda, R. O., Hart, P. E., & Stork, D. G. (2000). Pattern Classification. Wiley-Interscience (Vol. 24).
#'
#'Chen, Y., & Lin, C.-J. (2006). Combining SVMs with Various Feature Selection Strategies.
#'In I. Guyon, M. Nikravesh, S. Gunn, & L. A. Zadeh (Eds.), 
#'Feature Extraction: Foundations and Applications (Vol. 324, pp. 315-324). 
#'Berlin, Heidelberg: Springer Berlin Heidelberg.
#'
#' @export
fscore <- function(Data,classCol,featureCol,silent=FALSE){
  
  # Make the outcome factor anyways
  # Modified so that it works even with tibble; force the tibble to be a dataframe
  # This is not the best way to proceed; Ideally, all the code should be updated 
  # to work with tibble
  if("tbl_df" %in% class(Data)) Data <- as.data.frame(Data)
  
  
  if(!silent) cat("\nPerforming Feature selection f-score analysis \n\n")
  tr <- function(m) return(sum(diag(m)))
  # separate positive and negative feature sets
  posIns  <- Data[Data[,classCol]==unique(Data[,classCol])[1],]
  negIns  <- Data[Data[,classCol]==unique(Data[,classCol])[2],]
  f_score <- vector()
  featureColNames <- names(Data)[featureCol]

  for(feature in featureColNames){
    f_score[feature] = (norm(as.matrix(mean(posIns[,feature])-mean(negIns[,feature])))^2)/(tr(cov(as.matrix(posIns[,feature]))) + tr(cov(as.matrix(negIns[,feature]))))
  }
  # show only 2 significant digits
  f_score <- signif(f_score,2)
  return(f_score)
}
