#' Cross-validated Linear Discriminant Analysis
#'
#' A simple function to perform cross-validated Linear Discriminant Analysis
#' 
#' @param Data                 (dataframe) Data dataframe
#' @param classCol             (numeric)  column number that contains the variable to be predicted
#' @param selectedCols         (optional) (numeric)  all the columns of data that would be used either as predictor or as feature
#' @param extendedResults      (optional) (logical) Return extended results with model  and other metrics
#' @param SetSeed              (optional) (logical) Whether to setseed or not. use SetSeed to seed the random number generator to get consistent results; 
#'                             set false only for permutation tests
#'                             
#' @param cvType               (optional) (string) which type of cross-validation scheme to follow; One of the following values:
#'      \itemize{
#'      \item folds        =   (default) k-fold cross-validation 
#'      \item LOSO         =   Leave-one-subject-out cross-validation
#'      \item holdout      =   holdout Crossvalidation. Only a portion of data (cvFraction) is used for training.
#'      \item LOTO         =   Leave-one-trial out cross-validation.      
#'      }
#' @param ntrainTestFolds =    (optional) (parameter for only k-fold cross-validation) No. of folds for training and testing dataset
#' @param nTrainFolds     =    (optional) (parameter for only k-fold cross-validation) No. of folds in which to further divide Training dataset
#' @param modelTrainFolds =    (optional) (parameter for only k-fold cross-validation) specific folds from the first train/test split
#'  (ntrainTestFolds) to use for training 
#' @param foldSep              (numeric)  (parameter for only Leave-One_subject Out) mandatory column number for Leave-one-subject out cross-validation.
#' @param CV                   (optional) (logical) perform Cross validation of training dataset? 
#' If TRUE, posterior probabilites are present with the model
#' @param cvFraction           (optional) (numeric) Fraction of data to keep for training data
#' @param silent               (optional) (logical) whether to print messages or not
#' @param NewData              (optional) (dataframe) New Data frame features for which the class membership is requested  
#' @param ...                  (optional) additional arguments for the function
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
#' @return Depending upon \code{extendedResults}. \code{extendedResults}  = FALSE outputs Test accuracy \code{accTest} of discrimination; \code{extendedResults} = TRUE 
#' outputs Test accuracy \code{accTest} of discrimination, \code{ConfusionMatrixResults} Overall cross-validated confusion matrix results,\code{ConfMatrix} Confusion matrices
#' and \code{fitLDA} the fit cross-validated LDA model. If \code{CV} = TRUE , 
#' Posterior probabilities are generated and stored in the model.
#'
#'  
#' @examples
#' # simple model with holdout data partition of 80% and no extended results 
#' LDAModel <- LinearDA(Data = KinData, classCol = 1, 
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),cvType="holdout")
#' # Output:
#' #
#' # Performing Linear Discriminant Analysis
#' #
#' #
#' # Performing holdout Cross-validation
#' # 
#' # cvFraction was not specified,
#' #  Using default value of 0.8 (80%) fraction for training (cvFraction = 0.8)
#' # 
#' # Proportion of Test/Train Data was :  0.2470588 
#' # Predicted
#' # Actual  1  2
#' # 1 51 32
#' # 2 40 45
#' # [1] "Test holdout Accuracy is 0.57"
#' # holdout LDA Analysis: 
#' # cvFraction : 0.8 
#' # Test Accuracy 0.57
#' # *Legend:
#' # cvFraction = Fraction of data to keep for training data 
#' # Test Accuracy = mean accuracy from the Testing dataset
#' 
#' # alt uses:
#' # holdout cross-validation with 80% training data
#' LDAModel <- LinearDA(Data = KinData, classCol = 1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' CV=FALSE,cvFraction = 0.8,extendedResults = TRUE,cvType="holdout")
#'
#' # For a 10 fold cross-validation without outputting messages 
#' LDAModel <-  LinearDA(Data = KinData, classCol = 1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' extendedResults = FALSE,cvType = "folds",nTrainFolds=10,silent = TRUE)
#'
#' @import MASS caret
#' @author
#' Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#' \email{atesh.koul@@iit.it}
#' 
#' 
#' @references 
#' Fisher, R. A. (1936). The Use of Multiple Measurements in Taxonomic Problems. Annals of Eugenics, 7(2), 179-188.
#' 
#' Rao, C. (1948). The Utilization of Multiple Measurements in Problems of Biological Classification. 
#' In Journal of the Royal Statistical Society. Series B (Methodological) (Vol. 10, pp. 159-203).
#' 
#' @export
LinearDA <- function(Data,classCol,selectedCols,cvType,nTrainFolds,ntrainTestFolds,modelTrainFolds,foldSep,
                     CV=FALSE,cvFraction,extendedResults = FALSE,SetSeed=TRUE,
                     silent=FALSE,NewData=NULL,...){
  #simple function to perform linear discriminant analysis
  #library(MASS)
  #library(caret)
  
  
  
  
  if(missing(cvType)){
    if(!silent) cat("cvType was not specified, \n Using default k-folds Cross-validation \n")
    cvType <- "folds"
  }
  
  if(!(cvType %in% c("holdout","folds","LOSO","LOTO"))) stop(cat("\n cvType is not one of holdout,folds or LOSO. You provided",cvType))
  
  if(!silent) cat("\nPerforming Linear Discriminant Analysis \n\n")
  
  if(SetSeed)  set.seed(111)
  
  # set some checks
  # you haven't given any fraction for test, can't create data partition and get an accuracy
  # Perhaps, you have missed something?
  if(CV == TRUE & extendedResults==F) warning("No output requested. Please use extendedResults = TRUE to output the model")
  
  
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  selectedColNames <- names(Data)[selectedCols]
  
  # createDataPartition has different behavior for numeric and factor vectors
  # the random sampling is done within the levels of y when y is a factor in an 
  # attempt to balance the class distributions within the splits.
  # 
  # 
  # For numeric y, the sample is split into groups sections based on percentiles 
  # and sampling is done within these subgroups (see ?createDataPartition for more details)
  # 
  # Make the outcome factor anyways
  Data[,classCol] <- factor(Data[,classCol])
  
  
  switch(cvType,
         folds = {
           if(!silent) cat("\nPerforming k-fold Cross-validation \n\n")
           
           
           if(missing(ntrainTestFolds)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat(paste0("\nntrainTestFolds was not specified, \n Using default value of 10 (ntrainTestFolds = 10)\n"))
             ntrainTestFolds <- 10
           }
           
           if(missing(nTrainFolds)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat(paste0("nTrainFolds was not specified, \n Using default value of 10 fold cross-validation (nTrainFolds = 10)\n"))
             nTrainFolds <- 10
           }
           if(missing(modelTrainFolds)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat(paste0("\nmodelTrainFolds were not specified, \n Using default value of 1:",as.character(ntrainTestFolds-1),"\n"))
             modelTrainFolds <- 1:(ntrainTestFolds-1)
           }
           
           # get feature columns without response
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           trainTestIndex <- createFolds(Data[,classCol],list = FALSE,k = ntrainTestFolds)
           ModelTrainData <- Data[trainTestIndex %in% modelTrainFolds,]
           ModelTestData <- Data[!(trainTestIndex %in% modelTrainFolds),]
           
           trainIndexModel <- createFolds(ModelTrainData[,classCol],list = FALSE,k = nTrainFolds)
           
           #index <- createFolds(Data[,classCol],k,list = F)
           Foldacc <- rep(NA,nTrainFolds)
           accTestRun <- rep(NA,nTrainFolds)
           ConfMatrix <- list()
           for (i in 1:nTrainFolds){
             trainDataFold <- ModelTrainData[trainIndexModel!=i,]
             testDataFold <- ModelTrainData[trainIndexModel==i,]
             
             # CV = FALSE as we don't want leave-one-trial out cv
             fit <- lda(trainDataFold[,featureColNames],grouping = trainDataFold[,classCol],CV = FALSE,...)
             predictedTest <- predict(fit,newdata=testDataFold[,featureColNames])
             truthTest <- testDataFold[,classCol]
             Foldacc[i] <- sum(1 * (predictedTest$class==testDataFold[,classCol]))/length(predictedTest$class)
             
             predictedTest <- predict(fit,newdata=ModelTestData[,featureColNames])
             truthTest <- ModelTestData[,classCol]
             accTestRun[i] <- sum(1 * (predictedTest$class==ModelTestData[,classCol]))/length(predictedTest$class)
             ConfMatrix[[i]] <- confusionMatrix(truthTest,predictedTest$class)
             
             }
           # no sense in getting model from cross-validation
           fit <- NULL
           accTest <- mean(accTestRun,na.rm=T)
           
           if(!silent){
             print(table(truthTest,predictedTest$class,dnn = c("Actual","Predicted")))
             print(paste("The accuracy of discrimination was",signif(mean(Foldacc,na.rm=T),2)))
             
             print("Test Accuracies")
             print(table(truthTest,predictedTest$class,dnn = c("Actual","Predicted")))
             print(paste("The accuracy of discrimination was",signif(accTest,2)))
             # print parameters used
             cat("k-fold LDA Analysis:",'\nntrainTestFolds :', ntrainTestFolds,'\nmodelTrainFolds',modelTrainFolds,
                 "\nnTrainFolds:",nTrainFolds,"\nTest Accuracy",signif(mean(accTest),2))
             cat("\n*Legend:\nntrainTestFolds = No. of folds for training and testing dataset",
                 "\nmodelTrainFolds = Specific folds from the above ntrainTestFolds to use for training",
                 "\nnTrainFolds = No. of folds in which to further divide Training dataset",
                 "\nTest Accuracy = Mean accuracy from the Testing dataset\n")
           }
           
         },
         LOSO = {
           if(!silent) cat("\nPerforming Leave-one-subject-out Cross-validation \n\n")
           if(missing(foldSep)) stop("No foldSep provided")
           # get feature columns without response and Subject column (otherwise it will be used as a feature)
           featureColNames <- selectedColNames[!selectedColNames %in% c(names(Data)[classCol],names(Data[foldSep]))]
           
           Subs <- unique(Data[,foldSep])
           accTestRun <- rep(NA,length(Subs))
           ConfMatrix <- list()
           
           for (i in 1:length(Subs)){
             trainData <- Data[Data[,foldSep]!=Subs[i],]
             testData <- Data[Data[,foldSep]==Subs[i],]
             
             # CV = FALSE as we don't want leave-one-trial out cv
             fit <- lda(trainData[,featureColNames],grouping = trainData[,classCol],CV = FALSE,...)
             
             predictedTest <- predict(fit,newdata=testData[,featureColNames])
             truthTest <- testData[,classCol]
             accTestRun[i] <- sum(1 * (predictedTest$class==testData[,classCol]))/length(predictedTest$class)
             ConfMatrix[[i]] <- confusionMatrix(truthTest,predictedTest$class)
             
             if(!silent){
               # no need to pprint confusion matrices for each run
               #print(table(truthTest,predictedTest$class,dnn = c("Actual","Predicted")))
               #print(paste("The accuracy of discrimination in this run was",signif(mean(accTest[i],na.rm=T),2)))
               # print the confusion matrix
               # confusionMatrix(table(truth,predicted$class))
             }
             
           }
           
           
           
           # no sense in getting model from cross-validation
           fit <- NULL
           accTest <- mean(accTestRun,na.rm=T)
           if(!silent){
             # print parameters used
             cat("leave-one-subject-out LDA Analysis:",'\nnSubs :', length(Subs),"\nTest Accuracy",signif(accTest,2))
             cat("\n*Legend:\nnSubs = No. of Subjects in the data",
                 "\nTest Accuracy = Mean accuracy from the Testing dataset\n")
           }

         },
         holdout = {
           
           if(!silent) cat("\nPerforming holdout Cross-validation")
           
           if(missing(cvFraction)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat(paste0("\ncvFraction was not specified, \n Using default value of 0.8 (80%) fraction for training (cvFraction = 0.8)\n\n"))
             cvFraction <- 0.8
           }
           
           if(cvFraction==1 & CV == FALSE) warning("No fraction for train /test split")
           
           # get feature columns without response
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           # cross validate with 80% data in train set
           index <- createDataPartition(Data[,classCol],p=cvFraction,times=1)
           DataTrain <- Data[1:nrow(Data) %in% index$Resample1,]
           DataTest <- Data[!(1:nrow(Data) %in% index$Resample1),]
           if(!silent) cat("Proportion of Test/Train Data was : ",nrow(DataTest)/nrow(DataTrain),"\n")
           fit <- lda(DataTrain[,featureColNames],grouping = DataTrain[,classCol],CV = CV,...)
           # if CV = 
           if(!CV){
             predictedTest <- predict(fit,newdata=DataTest[,featureColNames])
             truthTest <- DataTest[,classCol]
             fit <- list(fit = fit,TestTruth = truthTest, TestPredicted = predictedTest)
             accTest <- sum(1 * (predictedTest$class==DataTest[,classCol]))/length(predictedTest$class)
             ConfMatrix <- confusionMatrix(truthTest,predictedTest$class)
             accTestRun <- NULL
             if(!silent){
               
               print(table(truthTest,predictedTest$class,dnn = c("Actual","Predicted")))
               print(paste("Test holdout Accuracy is ",signif(accTest,2)))
               # print the confusion matrix
               # confusionMatrix(table(truth,predicted$class))
               cat("holdout LDA Analysis:",'\ncvFraction :', cvFraction,"\nTest Accuracy",signif(mean(accTest),2))
               
               cat("\n*Legend:\ncvFraction = Fraction of data to keep for training data",
                   "\nTest Accuracy = Accuracy from the Testing dataset\n")
             }
           } else accTest <- NULL
         },
         LOTO = {
           if(!silent) cat("\nPerforming Leave-one-Trial-out Cross-validation \n (Might take some time depending upon the size of dataset)  \n")
           # get feature columns without response
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           
           index <- createFolds(Data[,classCol],k=nrow(Data),list=FALSE)
           accTestRun <- vector()
           ConfMatrix <- list()
           for(i in seq_along(index)){
             DataTrain <- Data[-i,]
             DataTest <-  Data[i,]
             fit <- lda(DataTrain[,featureColNames],grouping = DataTrain[,classCol],...)
             predictedTest <- predict(fit,newdata=DataTest[,featureColNames])
             truthTest <- DataTest[,classCol]
             #print(table(predicted$class,DataTest[,predictorCol]))
             accTestRun[i] <- sum(1 * (predictedTest$class==DataTest[,classCol]))/length(predictedTest$class)
             ConfMatrix[[i]] <- confusionMatrix(truthTest,predictedTest$class)
           }
           # no sense in getting model from cross-validation
           fit <- NULL
           accTest <- mean(accTestRun,na.rm = TRUE)
           
           if(!silent){
             print(paste("Test LOTO classification Accuracy is ",signif(accTest,2)))
             # print parameters used
             cat("leave-one-Trial-out LDA Analysis:","\nTest Accuracy",signif(accTest,2))
             cat("\n*Legend:\nTest Accuracy = Mean accuracy from the Testing dataset\n")
           }
           
           }
  )
  
  # get overall confusion Matrix results
  ConfusionMatrixResults <- overallConfusionMetrics(ConfMatrix)
  
  if(!is.null(NewData) & (!CV)){
    newDataprediction <- predictNewData(fit$fit,NewData)
    fit <- list(fit=fit,newDataprediction=newDataprediction)
  }
  
  if(extendedResults){
    ResultsLDA <- list(fitLDA = fit,accTest = accTest,ConfMatrix=ConfMatrix,ConfusionMatrixResults=ConfusionMatrixResults,accTestRun= accTestRun)
    return(ResultsLDA)
  }else return(accTest)
  
}
