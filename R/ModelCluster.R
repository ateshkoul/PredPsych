#' Model based Clustering
#'
#' A simple function to perform Model based cluster Analysis
#' :
#' @param Data    (dataframe) Data dataframe
#' @param NewData (optional) (dataframe) New Data frame for which the class membership is requested
#' @param G       (optional) (numeric) No. of components to verify
#' @param silent  (optional) (logical) whether to print messages or not
#' @param ...     (optional) additional arguments for the function
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
#'# not run
#'# cluster_time <- ModelCluster(KinData[,c(2,12,22,32,42,52,62,72,82,92,102,112)],G=1:12)
#'# Output:
#'# Performing Cluster analysis 
#'# --cluster Results --
#' @import mclust
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#'
#'
#'@references 
#'
#'Han, J., Kamber, M., & Pei, J. (2012). Cluster Analysis. In Data Mining (pp. 443-495). Elsevier.
#'
#'Fraley, C., & Raftery, a E. (1998). How Many Clusters? Which Clustering Method? Answers Via Model-Based Cluster Analysis.
#'The Computer Journal, 41(8), 578-588.
#'
#' @export
ModelCluster <- function(Data,NewData=NULL,G,silent=FALSE,...){
  #library(mclust)
  if(!silent) cat("\nPerforming Cluster analysis \n\n")

  set.seed(111)
  model <- Mclust(Data,...)
  print(paste("optimal number of clusters are",model$G))
  if(!is.null(NewData)) {
    classMembership <- predict(model,NewData)
    return(classMembership)
  }else return(model)
}