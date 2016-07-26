#' Cross-validated Linear Discriminant Analysis
#'
#' A simple function to perform cross-validated Linear Discriminant Analysis
#' 
#' @param Data                 (dataframe) Data dataframe
#' @param predictorCol         (numeric)  column number that contains the variable to be predicted
#' @param selectedCols         (optional) (numeric)  all the columns of data that would be used either as predictor or as feature
#' @param cvType               (optional) (string) Cross validation type.
#' @return \code{accuracy} of discrimination
#'
#'@author
#'Atesh Koul, C'MON group, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
LinearDA <- function(Data,predictorCol,selectedCols,cvType="createDataPartition"){
  #simple function to perform linear discriminant analysis
  library(MASS)
  library(caret)
  set.seed(111)
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  if(missing(cvType))  cvType = "createDataPartition"
  
  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(Data)[predictorCol],selectedColNames)]

  Data[,predictorCol] <- factor(Data[,predictorCol])
  if (cvType=="createDataPartition"){
    
    # cross validate with 80% data in train set
    index <- createDataPartition(Data[,predictorCol],p=0.8,times=1)
    DataTrain <- Data[1:nrow(Data) %in% index$Resample1,]
    DataTest <- Data[!(1:nrow(Data) %in% index$Resample1),]
    fit <- lda(DataTrain[,featureColNames],grouping = DataTrain[,predictorCol])
    predicted <- predict(fit,newdata=DataTest[,featureColNames])
    # print the confusion matrix
    print(table(DataTest[,predictorCol],predicted$class,dnn = c("Actual","Predicted")))
    Acc <- sum(1 * (predicted$class==DataTest[,predictorCol]))/length(predicted$class)
    print(paste("The accuracy of discrimination was",signif(Acc,2)))
  }else if(cvType=="LOTO"){
    index <- createFolds(Data[,predictorCol],k=nrow(Data),list=FALSE)
    acc <- vector()
    for(i in seq_along(index)){
      DataTrain <- Data[-i,]
      DataTest <-  Data[i,]
      fit <- lda(DataTrain[,featureColNames],grouping = DataTrain[,predictorCol])
      predicted <- predict(fit,newdata=DataTest[,featureColNames])
      #print(table(predicted$class,DataTest[,predictorCol]))
      acc[i] <- sum(1 * (predicted$class==DataTest[,predictorCol]))/length(predicted$class)
    }
    Acc <- mean(acc)
    print(paste("The accuracy of discrimination was",signif(Acc,2)))
  }  
  
  return(Acc)


}
