#' Model based Clustering
#'
#' A simple function to perform Model based cluster Analysis
#' :
#' @param Data    (dataframe) Data dataframe
#' @param NewData (optional) (dataframe) New Data frame for which the class membership is requested
#' @param G       (optional) (numeric) No. of components to verify
#'
##'@return \code{class membership} of the clustered \code{NewData} 
#'
#'@examples
#'# clustering kinematics data at 10% of movement
#'cluster_time <- ModelCluster(KinData[,c(2,12,22,32,42,52,62,72,82,92,102,112)],G=1:12)
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
ModelCluster <- function(Data,NewData=NULL,G,...){
  library(mclust)
  set.seed(111)
  model <- Mclust(Data,...)
  print(paste("optimal number of clusters are",model$G))
  if(!is.null(NewData)) {
    classMembership <- predict(model,NewData)
    return(classMembership)
  }else return(model)
}