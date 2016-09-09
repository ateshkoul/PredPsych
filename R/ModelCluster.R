#' Model based Clustering
#'
#' A simple function to perform Model based cluster Analysis
#' :
#' @param Data    (dataframe) Data dataframe
#' @param NewData (optional) (dataframe) New Data frame for which the class membership is requested
#' @param G       (optional) (numeric) No. of components to verify
#'
#' @details 
#' The function implements Model based clustering in predictive framework. Model based clustering approaches provide a
#' structured way of choosing number of clusters (C. Fraley & Raftery, 1998). 
#' Data are considered to be generated from a set of Gaussian distributions (components or clusters) 
#' i.e. as a mixture of these components (mixture models). Instead of using heuristics, model based 
#' clustering approximates Bayes factor (utilizing Bayesian information Criterion) to determine the model 
#' with the highest evidence (as provided by the data).
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
#' @export
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