#'simple function to perform Model based cluster Analysis
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item Data = Data dataframe
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item class membership 
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
ModelCluster <- function(clustData,NewData){
  library(mclust)
  model <- Mclust(Data)
  print(paste("optimal number of clusters are"),model)
  classMembership <- predict(NewData)
  return(classMembership)
}