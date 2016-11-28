#' Generic Dimensionallity Reduction Function
#' 
#'
#' A simple function to perform dimensionality reduction
#' 
#' @param Data         (dataframe) a data frame with variable/feature columns
#' @param selectedCols  (optional)(numeric) which columns should be treated as data(features/columns) (defaults to all columns)
#' @param outcome     (optional)(vector) optional vector for visualising plots
#' @param plot         (optional)(logical) To plot or not to plot
#' 
#' @details 
#' Dimensionality Reduction is the process of reducing the dimensions of the dataset. 
#' Multivariate data, even though are useful in getting an overall understanding of the underlying phenomena,
#' do not permit easy interpretability. Moreover, variables in such data often are correlated with each other
#' .For these reasons, it might be imperative to reduce the dimensions of the data. 
#' Various models have been developed for such dimensionality reduction. Of these, MDS and PCA has been 
#' demonstrated in the current implementation.
#' 
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
#' @export
DimensionRed <- function(Data,method="MDS",selectedCols,outcome=NA,plot=FALSE,...){
  # if nothing specific is provided, default to all the columns
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  if(plot & any(is.na(outcome)))  cat("Perhaps you forgot the classCol vector \nPlease enter the classCol vector for the plot")
  
  
  switch(method,
         MDS = {
           DistanceMatrix <- dist(Data[,selectedCols])
           multiDimScale <- cmdscale(DistanceMatrix,...)
           if(plot & any(!is.na(outcome))) {
             # update data for plotting
             library(ggplot2)
             plotData <- data.frame(Dimension_1 = multiDimScale[,1],Dimension_2=multiDimScale[,2],outcome = factor(outcome))
             p <- ggplot(plotData,aes(x=Dimension_1,y=Dimension_2,col=outcome))+
               geom_point()+theme_bw(base_size = 18)+scale_color_grey(end = 0.7)+
               theme(panel.grid = element_blank())+stat_ellipse(type = 'norm')#+guides(col=FALSE)
             print(p)
           }
           
           return(multiDimScale)
         },PCA = {
           principal <- princomp(Data[,selectedCols],...)
           print(summary(principal)) # print variance accounted for 
           if(plot) plot(principal,type="lines") # scree plot 
           return(principal)
         }
         
         )
  
  
  
}
