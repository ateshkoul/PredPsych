#' Cross-validated Linear Discriminant Analysis
#'
#' A simple function to perform cross-validated Linear Discriminant Analysis
#' 
#' @param Data                 (dataframe) Data dataframe
#' @param predictorCol         (numeric)  column number that contains the variable to be predicted
#' @param selectedCols         (optional) (numeric)  all the columns of data that would be used either as predictor or as feature
#' @param CV                   (optional) (logical) perform Cross validation of training dataset? 
#' If TRUE, posterior probabilites are present with the model
#' @param cvFraction           (optional) (numeric) Fraction of data to keep for training data
#' @param extendedResults      (optional) (logical) Return extended results with model?
#' 
#' @details 
#' The function implements Linear Disciminant Analysis, a simple algorithm for classification based analyses
#' .LDA builds a model composed of a number of discriminant functions based on linear combinations of 
#' data features that provide the best discrimination between two or more conditions/classes. 
#' The aim of the statistical analysis in LDA is thus to combine the data features scores in a way that 
#' a single new composite variable, the discriminant function, is produced (for details see Fisher, 1936;
#'  Rao, 1948)). 
#' 
#' 
#' @return Depending upon \code{extendedResults}. \code{extendedResults} FALSE =  \code{Acc} of discrimination () \code{extendedResults} TRUE 
#' \code{Acc} Accuracy of discrimination and \code{fitLDA} the fit cross-validated LDA model. If \code{CV} = TRUE , 
#' Posterior probabilities are generated and stored in the model
#'
#'  
#' @examples
#' # simple model with data partition of 80% and no extended results 
#' LDAModel <- LinearDA(Data = KinData, predictorCol = 1, selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112))
#' #outout
#' #       Predicted
#' #Actual  1  2
#' #1 51 32
#' #2 40 45
#' #"The accuracy of discrimination was 0.57"
#'
#' LDAModel <- LinearDA(Data = KinData, predictorCol = 1, selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' CV=FALSE,cvFraction = 0.8,extendedResults = TRUE)
#'
#'
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#' @export
LinearDA <- function(Data,predictorCol,selectedCols,CV=FALSE,cvFraction=0.8,extendedResults = FALSE,SetSeed=TRUE,cvType="createDataPartition",k=10,...){
  #simple function to perform linear discriminant analysis
  library(MASS)
  library(caret)
  if(SetSeed)  set.seed(111)
  
  # set some checks
  # you haven't given any fraction for test, can't create data partition and get an accuracy
  # Perhaps, you have missed something?
  if(cvFraction==1 & CV == FALSE) warning("No fraction for train /test split") 
  if(CV == TRUE & extendedResults==F) warning("No output requested. Please use extendedResults = TRUE to output the model")
  
  
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(Data)[predictorCol],selectedColNames)]
  
  Data[,predictorCol] <- factor(Data[,predictorCol])
  switch(cvType,
         createDataPartition = {
           # cross validate with 80% data in train set
           index <- createDataPartition(Data[,predictorCol],p=cvFraction,times=1)
           DataTrain <- Data[1:nrow(Data) %in% index$Resample1,]
           DataTest <- Data[!(1:nrow(Data) %in% index$Resample1),]
           cat("Proportion of Test/Train Data was : ",nrow(DataTest)/nrow(DataTrain),"\n")
         },
         Folds = {
           index <- createFolds(Data$L1,k,list = F)
           DataTrain <- Data[index %in% c(1:k-1),]
           DataTest <- Data[index %in% k,]
           cat("Proportion of Test/Train Data was : ",nrow(DataTest)/nrow(DataTrain),"\n")
         }
  )
  
  fit <- lda(DataTrain[,featureColNames],grouping = DataTrain[,predictorCol],CV = CV,...)
  # if CV = 
  if(!CV){
    predicted <- predict(fit,newdata=DataTest[,featureColNames])
    truth <- DataTest[,predictorCol]
    fit <- list(fit = fit,TestTruth = truth, TestPredicted = predicted)
    print(table(truth,predicted$class,dnn = c("Actual","Predicted")))
    Acc <- sum(1 * (predicted$class==DataTest[,predictorCol]))/length(predicted$class)
    print(paste("The accuracy of discrimination was",signif(Acc,2)))
    # print the confusion matrix
    # confusionMatrix(table(truth,predicted$class))
  } else Acc <- NULL
  
  if(extendedResults){
    ResultsLDA <- list(fitLDA = fit,Acc = Acc)
    return(ResultsLDA)
  }else return(Acc)
  
}
