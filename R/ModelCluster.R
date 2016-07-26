#' Model based Clustering
#'
#' A simple function to perform Model based cluster Analysis
#' :
#' @param Data    (dataframe) Data dataframe
#' @param NewData (optional) (dataframe) New Data frame for which the class membership is requested
#'
##'@return \code{class membership} of the clustered \code{NewData} 
#'
#'
#'@author
#'Atesh Koul, C'MON group, Istituto Italiano di technologia
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