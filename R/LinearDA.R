#'simple function to perform cross-validated Linear Discriminant Analysis
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item Data = Data dataframe
#'      \item predictorCol(numeric) = column number that contains the variable to be predicted
#'      \item selectedCols(numeric) = all the columns of data that would be used either as predictor or as feature
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item accuracy of discrimination
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
LinearDA <- function(Data,predictorCol,selectedCols){
  #simple function to perform linear discriminant analysis
  library(MASS)
  library(caret)
  set.seed(111)
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))

  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(Data)[predictorCol],selectedColNames)]


  Data[,predictorCol] <- factor(Data[,predictorCol])
  # cross validate with 80% data in train set
  index <- createDataPartition(Data[,predictorCol],p=0.8,times=1)
  DataTrain <- KinData[1:nrow(Data) %in% index$Resample1,]
  DataTest <- KinData[!(1:nrow(Data) %in% index$Resample1),]
  fit <- lda(DataTrain[,featureColNames],grouping = DataTrain[,predictorCol])
  predicted <- predict(fit,newdata=DataTest[,featureColNames])
  print(table(predicted$class,DataTest[,predictorCol]))
  acc <- sum(1 * (predicted$class==DataTest[,predictorCol]))/length(predicted$class)
  print(paste("The accuracy of discrimination was",signif(acc,2)))


}
