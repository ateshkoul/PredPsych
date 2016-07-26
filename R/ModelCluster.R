#'simple function to perform Model based cluster Analysis
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item Data = Data dataframe
#'}
#'}
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item class membership 
#'}
#'}
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
ModelCluster <- function(Data,NewData=NULL,...){
  library(mclust)
  set.seed(111)
  model <- Mclust(Data,...)
  print(paste("optimal number of clusters are",model$G))
  if(!is.null(NewData)) {
    classMembership <- predict(model,NewData)
    return(classMembership)
  }else return(model)
}