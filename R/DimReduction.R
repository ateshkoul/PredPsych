#' Generic Dimensionallity Reduction Function
#' 
#'
#' A simple function to perform dimensionality reduction
#' 
#' @param Data         (dataframe) a data frame with variable/feature columns
#' @param selectedCol  (optional)(numeric) which columns should be treated as data(features/columns) (defaults to all columns)
#'          
#' @return  Data frame with \code{Results}
#'
#'@examples
#'# reducing dimension of Grip aperture from 10 to 2
#'GripAperture <- DimensionRed(KinData,selectedCols = 12:21,outcome = KinData[,"Object.Size"],plot = TRUE)
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
DimensionRed <- function(Data,method="MDS",selectedCols,outcome=NA,plot=FALSE,...){
  # if nothing specific is provided, default to all the columns
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  if(plot & any(is.na(outcome)))  cat("Perhaps you forgot the outcome vector \nPlease enter the outcome vector for the plot")
  
  
  switch(method,
         MDS = {
           DistanceMatrix <- dist(Data[,selectedCols])
           MDS <- cmdscale(DistanceMatrix,...)
           if(plot & any(!is.na(outcome))) {
             # plot(MDS,type='n')
             # text(MDS[,1],MDS[,2],labels=names(Data[,selectedCols]))
             # update data for plotting
             library(ggplot2)
             plotData <- data.frame(MDS,outcome = factor(outcome))
             p <- ggplot(plotData,aes(x=X1,y=X2,col=outcome))+
               geom_point()+theme_bw()+scale_color_grey(end = 0.7)+
               theme(panel.grid = element_blank())+stat_ellipse(type = 'norm')#+guides(col=FALSE)
             print(p)
           }
           
           return(MDS)
         }
         
         
         )
  
  
  
}
