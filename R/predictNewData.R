#' Predict Class membership for New Data
#'
#' A simple function to predict class membership for new data
#' 
#' @param model      (model)     Classifier model obtained from a classification analysis 
#' @param NewData    (optional) (dataframe) New Data frame features for which the class membership is requested 
#' @param ...                  (optional) additional arguments for the function
#' @details 
#'  A function to generate predictions on a new dataset based on a previously estimated classifier model.
#'  This could be generated from LinearDA, classifyFun or DTMOdel functions.
#'  
#' @return Predictions for each case in the NewData. 
#'
#' @import MASS e1071
#' @author
#' Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#' \email{atesh.koul@@iit.it}
#' 
#' @export
predictNewData <- function(model,NewData,...){
  
  #ensure the data exists
  if(!is.null(NewData)){
    # generate prediction for the cases based on the model class
    # ... allows for various kinds of inputs
    # 
    # The predict funcion redirects based on the class of the object
    # predict as a function thus becomes variable in it's output and 
    # arguments that it can take. 
    # That is why, it has to be customised for each kind of classifier
    # Here the best way to output the predictions is as class predictions
    # if you use type='vector', the output differs and is not a factor
    # This doesn't work for confusion matrix especially for LOTO as only 
    # 1 prediction is made at a time
    newDataprediction <- predict(model,NewData,...)
    
  }
  return(newDataprediction)
}