#' Generic Dimensionallity Reduction Function
#' 
#'
#' A simple function to perform dimensionality reduction
#' 
#' @param Data          (dataframe)  a data frame with variable/feature columns
#' @param method        (optional) (character) Dimensionality reduction method to be used
#' @param selectedCols  (optional)(numeric) which columns should be treated as data(features/columns) (defaults to all columns)
#' @param outcome       (optional)(vector) optional vector for visualising plots
#' @param plot          (optional)(logical) To plot or not to plot
#' @param silent        (optional) (logical) whether to print messages or not
#' @param ...           (optional) additional arguments for the function
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
#'GripAperture <- DimensionRed(KinData,selectedCols = 12:21,
#'outcome = KinData[,"Object.Size"],plot = TRUE)
#'
#'@import stats
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#'
#'@references 
#'Bishop, C. M. (2006). Pattern Recognition and Machine Learning. (M. Jordan, J. Kleinberg, & B. Scholkopf, Eds.) 
#'(1st ed.). Springer-Verlag New York.
#'
#'Cox, T. F., & Cox, M. A. A. (2000). Multidimensional scaling (Second ed.). Chapman & Hall/CRC.
#'
#' @export
DimensionRed <- function(Data,method="MDS",selectedCols,outcome=NA,plot=FALSE,silent=FALSE,...){
  # if nothing specific is provided, default to all the columns
  if(!silent) cat("\nPerforming Dimensionality Reduction analysis \n\n")
  
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  if(plot & any(is.na(outcome)))  cat("Perhaps you forgot the classCol vector \nPlease enter the classCol vector for the plot")
  # Modified so that it works even with tibble; force the tibble to be a dataframe
  # This is not the best way to proceed; Ideally, all the code should be updated 
  # to work with tibble
  # make it a bit generic to handle matrices as well along with a warning 
  permittedDataClass <- c("tbl_df","matrix")
  if(any(permittedDataClass %in% class(Data))){
    warning(cat("the data entered is of the class ",class(Data),". Coersing it to be a dataframe. Check results"))
    Data <- as.data.frame(Data)
  }
  
  switch(method,
         MDS = {
           DistanceMatrix <- dist(Data[,selectedCols])
           multiDimScale <- cmdscale(DistanceMatrix,...)
           if(plot & any(!is.na(outcome))) {
             # update data for plotting
             #library(ggplot2)
             # to avoid R CMD check note
             Dimension_1 <- Dimension_2 <- NULL
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
