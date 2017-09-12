#' Generic Classification Analyses
#'
#' function for performing generic classification Analysis
#' 
#' @param Data              (dataframe) dataframe of the data
#' @param classCol          (numeric) column number that contains the variable to be predicted
#' @param selectedCols      (optional) (numeric) all the columns of data that would be used either as predictor or as feature
#' @param cvType            (optional) (string) which type of cross-validation scheme to follow; One of the following values:
#'      \itemize{
#'      \item folds       =  (default) k-fold cross-validation 
#'      \item LOSO        =  Leave-one-subject-out cross-validation
#'      \item holdout     =  holdout Crossvalidation. Only a portion of data (cvFraction) is used for training.
#'      \item LOTO        =  Leave-one-trial out cross-validation.
#'      }
#' @param ntrainTestFolds    (optional) (parameter for only k-fold cross-validation) No. of folds for training and testing dataset
#' @param nTrainFolds        (optional) (parameter for only k-fold cross-validation) No. of folds in which to further divide Training dataset
#' @param modelTrainFolds =  (optional) (parameter for only k-fold cross-validation) specific folds from the first train/test split
#'  (ntrainTestFolds) to use for training
#' @param nTuneFolds         (optional) (parameter for only k-fold cross-validation) No. of folds for Tuning
#' @param tuneFolds          (optional) (parameter for only k-fold cross-validation) specific folds from the above nTuneFolds to use for tuning
#' @param foldSep            (numeric)  (parameter for only Leave-One_subject Out) mandatory column number for Leave-one-subject out cross-validation.
#' @param cvFraction         (optional) (numeric) Fraction of data to keep for training data
#' @param classifierName     (optional) (string) name of the classifier to be used
#' @param genclassifier      (optional) (function or string) a classifier function or a name (e.g. Classifier.svm)
#' @param ranges             (optional) (list)  ranges for tuning support vector machine
#' @param tune               (optional) (logical) whether tuning of svm parameters should be performed or not
#' @param cost               (optional) (numeric) regularization parameter of svm
#' @param gamma              (optional) (numeric)  rbf kernel parameter
#' @param silent             (optional) (logical) whether to print messages or not
#' @param extendedResults    (optional) (logical) Return extended results with model and other metrics
#' @param SetSeed            (optional) (logical) Whether to setseed or not. use SetSeed to seed the random number generator to get consistent results; 
#'                                      set false only for permutation tests
#' @param NewData            (optional) (dataframe) New Data frame features for which the class membership is requested                                   
#' @param ...                (optional) additional arguments for the function                             
#' 
#' @details 
#' This function implements Classification Analysis. 
#' Classification Analysis is a supervised machine learning approach that attempts to identify 
#' holistic patters in the data and assign to it classes (classification). Given a set of features, 
#' a classification analysis automatically learns intrinsic patterns in the data to be able to predict 
#' respective classes. If the data features are informative about the classes, a high classification score
#' would be achieved. 
#' 
#' 
#' @return Depending upon \code{extendedResults}. \code{extendedResults}  = FALSE outputs Test accuracy \code{accTest} of discrimination; \code{extendedResults} = TRUE 
#' outputs Test accuracy \code{accTest} of discrimination, \code{accTestRun} discrimination for each run in case of cvType as LOSO,LOTO or Folds \code{ConfMatrix} Confusion matrices and \code{classificationResults} list of the cross-validation results including the model 
#' and  \code{ConfusionMatrixResults} Overall cross-validated confusion matrix results  
#'
#' @examples
#' # classification analysis with SVM
#' Results <- classifyFun(Data = KinData,classCol = 1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),cvType="holdout")
#' 
#' # Output:
#' 
#' # Performing Classification Analysis
#' #
#' # Performing holdout Cross-validation
#' # genclassifier was not specified, 
#' #   Using default value of Classifier.svm (genclassifier = Classifier.svm)"
#' # 
#' # cvFraction was not specified, 
#' #  Using default value of 0.8 (cvFraction = 0.8)
#' # 
#' # Proportion of Test/Train Data was :  0.2470588 
#' # [1] "Test holdout Accuracy is  0.65"
#' # holdout classification Analysis: 
#' # cvFraction : 0.8 
#' # Test Accuracy 0.65
#' # *Legend:
#' # cvFraction = Fraction of data to keep for training data 
#' # Test Accuracy = Accuracy from the Testing dataset
#' 
#' # Alternate uses:
#' # perform a k-folds cross-validated classification analysis:
#' Results <- classifyFun(Data = KinData,classCol = 1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),cvType = "folds")
#' 
#' # use extendedResults as well as tuning
#' Results <- classifyFun(Data = KinData,classCol = 1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' cvType = "folds",extendedResults = TRUE,tune=TRUE)
#'
#'
#'
#' @import e1071 caret
#' @author
#' Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#' 
#' \email{atesh.koul@@iit.it}
#' 
#' @references 
#' Duda, R. O., Hart, P. E., & Stork, D. G. (2000). Pattern Classification. Wiley-Interscience (Vol. 24).
#' 
#' Vapnik, V. (1995). The Nature of statistical Learning Theory. Springer-Verlag New York.
#' 
#' Hsu, C. C., Chang, C. C., & Lin, C. C. (2003). A practical guide to support vector classification, 1(1), 1-16. 
#' 
#' @export
classifyFun <- function(Data,classCol,selectedCols,cvType,ntrainTestFolds,nTrainFolds,modelTrainFolds,nTuneFolds,tuneFolds,foldSep,cvFraction,
                        ranges=NULL,tune=FALSE,cost=1,gamma=0.5,classifierName='svm',genclassifier,silent=FALSE,extendedResults = FALSE,
                        SetSeed=TRUE,NewData=NULL,...){

  
  #library(e1071)
  #library(caret)
  # dont use a constant set.seed with permutation testing
  # u will get a constant accuracy!!
  #set.seed(123)
  
  if(missing(cvType)){
    if(!silent) cat("cvType was not specified, \n Using default k-folds Cross-validation \n")
    cvType <- "folds"
  }
  
  if(!(cvType %in% c("holdout","folds","LOSO","LOTO"))) stop(cat("\n cvType is not one of holdout,folds or LOSO. You provided",cvType))
  
  
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  
  if(!silent) cat("\nPerforming Classification Analysis \n\n")
  
  # get the features:  in case u enter names of columns, it works anyways
  ifelse(is.character(selectedCols),selectedColNames <- selectedCols,selectedColNames <- names(Data)[selectedCols])
  
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
  
  
  # get feature columns without response
  featureColNames <- selectedColNames[-match(names(Data)[classCol],selectedColNames)]
  predictorColNames <- names(Data)[classCol]
  
  
  

  # if predictor has missing, remove those columns
  if(sum(is.na(Data[,predictorColNames]))>0) Data <- Data[!is.na(Data[,predictorColNames]),]
  # set seed; skip if explicitly is made false (e.g. in Permutation Testing)
  if(SetSeed)  set.seed(111)
  
  switch(cvType,
         folds = {
           if(!silent) cat("\nPerforming k-fold Cross-validation")
           if(missing(nTrainFolds)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat(paste0("\nnTrainFolds was not specified, \n Using default value of 10 fold cross-validation (nTrainFolds = 10)\n"))
             nTrainFolds <- 10
           }
           
           if(missing(ntrainTestFolds)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat(paste0("\nntrainTestFolds was not specified, \n Using default value of 10 (ntrainTestFolds = 10)\n"))
             ntrainTestFolds <- 10
           }
           
           if(missing(modelTrainFolds)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat(paste0("\nmodelTrainFolds were not specified, \n Using default value of 1:",as.character(ntrainTestFolds-1),"\n"))
             modelTrainFolds <- 1:(ntrainTestFolds-1)
           }
           
           if(missing(genclassifier)) 
           
           if(missing(genclassifier)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat("\ngenclassifier was not specified,\n Using default value of Classifier.svm.Folds (genclassifier = Classifier.svm.Folds)\n")
             genclassifier <- Classifier.svm.Folds
             
           }
           
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           if(tune) {
             
             if(!(classifierName %in% c('svm','knn3'))){
               stop(paste0("Tuning is currently implemented for only svm and knn3. You input the classifiername as ",classifierName))
             }
             
             # use stratified cross validation instead
             # divide the data into 3 parts:
             # 1. for tuning parameters
             # 2. for training model
             # 3. for testing prediction (on a data that it has never seen ever)
             #
             # Keeping more for tuning
             
             if(missing(nTuneFolds)){
               if(!silent) cat(paste0("\nnTuneFolds was not specified, Using default value of 3 (nTuneFolds = 3)"))
               nTuneFolds <- 3
             }
             
             if(missing(tuneFolds)){
               if(!silent) cat(paste0("\ntuneFold was not specified, Using default value of 1 (tuneFolds = 1)\n\n"))
               tuneFolds <- 1
               
             } 
             
             
             trainIndexOverall <- createFolds(Data[,classCol],list = FALSE,k = nTuneFolds)
             # leave first part for tuning the classifier
             tuneTrainData <- Data[trainIndexOverall %in% tuneFolds,]
             ModelData <- Data[!(trainIndexOverall %in% tuneFolds),]
             
             #ModelTestData <- Data[trainIndexOverall==3,]
             obj <- getTunedParam(tuneTrainData,classCol,classifierName,featureColNames,ranges,silent)["best.parameters"]
             
           } else{
             # folds = 2
             # trainIndexOverall <- createFolds(Data[,predictorColNames],list = FALSE,k = folds)
             # ModelTrainData <- Data[trainIndexOverall==1,]
             # ModelTestData <- Data[trainIndexOverall==2,]
             ModelData <- Data
             obj <- data.frame(gamma=gamma,cost=cost)
           }
           
           trainTestIndex <- createFolds(ModelData[,classCol],list = FALSE,k = ntrainTestFolds)
           ModelTrainData <- ModelData[trainTestIndex %in% modelTrainFolds,]
           ModelTestData <- ModelData[!(trainTestIndex %in% modelTrainFolds),]
           
           
           #initialising vectors
           acc <- rep(NA,nTrainFolds)
           # container to collect test accuracy from multiple runs
           accTestRun <- rep(NA,nTrainFolds)
           
           trainIndexModel <- createFolds(ModelTrainData[,predictorColNames],list = FALSE,k = nTrainFolds)
           
           if(!silent){
             print('Begining k-fold Classification')
           }
           
           # define the holder for classification results
           classificationResults <- list()
           ConfMatrix <- list()
           
           for (i in 1:nTrainFolds){
             trainDataFold <- ModelTrainData[!trainIndexModel==i,]
             testDataFold <- ModelTrainData[trainIndexModel==i,]
             
             # the classifier has generic
             # generic error function:
             classificationResults[[i]] <- do.call(genclassifier,c(list(trainData=trainDataFold,testData=testDataFold,ModelTestData=ModelTestData,predictorColNames=predictorColNames,
                                                                        featureColNames=featureColNames,expand.grid(obj),...)))
             acc[i] = classificationResults[[i]][["acc"]]
             accTestRun[i] = classificationResults[[i]][["accTest"]]
             ConfMatrix[[i]] <- classificationResults[[i]][["ConfMatrix"]]
           }
           accTest <- mean(accTestRun,na.rm=T)
           
           if(!silent){
             print(paste("Mean CV Accuracy",signif(mean(acc),2)))
             print(paste("Mean Test Accuracy",signif(mean(accTest),2)))
             
             # print parameters used
             cat("k-fold classification Analysis:",'\nntrainTestFolds :', ntrainTestFolds,'\nmodelTrainFolds',modelTrainFolds,
                 "\nnTrainFolds:",nTrainFolds,"\nTest Accuracy",signif(accTest,2))
             cat("\n*Legend:\nntrainTestFolds = No. of folds for training and testing dataset",
                 "\nmodelTrainFolds = Specific folds from the above nTrainFolds to use for training",
                 "\nnTrainFolds = No. of folds in which to further divide Training dataset",
                 "\nTest Accuracy = Mean accuracy from the Testing dataset\n")
             
           }
         },
         LOSO = {
           if(!silent) cat("\nPerforming Leave-one-subject-out Cross-validation \n\n")
           if(missing(foldSep)) stop("No foldSep provided")
           
           
           if(tune) warning("\n\n No tuning performed. Tuning is implemented for only k-fold analsyis (for now) \n\n")
           
           if(missing(genclassifier)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat("\ngenclassifier was not specified, \n Using default value of Classifier.svm (genclassifier = Classifier.svm)\n")
             genclassifier <- Classifier.svm
             
           }
           
           featureColNames <- selectedColNames[!selectedColNames %in% c(names(Data)[classCol],names(Data[foldSep]))]
           
           Subs <- unique(Data[,foldSep])
           obj <- data.frame(gamma=gamma,cost=cost)
           accTestRun <- rep(NA,length(Subs))
           
           # define the holder for classification results
           classificationResults <- list()
           ConfMatrix <- list()
           for (i in 1:length(Subs)){
             trainDataSub <- Data[Data[,foldSep]!=Subs[i],]
             testDataSub <- Data[Data[,foldSep]==Subs[i],]
             
             
             
             classificationResults[[i]] <- do.call(genclassifier,c(list(trainData=trainDataSub,testData=testDataSub,predictorColNames=predictorColNames,
                                                       featureColNames=featureColNames,expand.grid(obj),...)))
             accTestRun[i] <- classificationResults[[i]][["accTest"]]
             ConfMatrix[[i]] <- classificationResults[[i]][["ConfMatrix"]]
             
           }
           
           accTest <- mean(accTestRun,na.rm=TRUE)
           if(!silent){
             print(paste("Mean Test Leave-one-Subject-out Accuracy is ",signif(accTest,2)))
             # print parameters used
             cat("leave-one-subject-out classification Analysis:",'\nnSubs :', length(Subs),"\nTest Accuracy",signif(accTest,2))
             cat("\n*Legend:\nnSubs = No. of Subjects in the data",
                 "\nTest Accuracy = Mean accuracy from the Testing dataset\n")
           }
           
         },
         holdout = {
           
           if(!silent) cat("\nPerforming holdout Cross-validation")
           if(tune) warning("\n\n No tuning performed. Tuning is implemented for only k-fold analsyis (for now) \n\n")
           if(missing(genclassifier)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat("\ngenclassifier was not specified, \n Using default value of Classifier.svm (genclassifier = Classifier.svm)\n")
             genclassifier <- Classifier.svm
             
           } 
           if(missing(cvFraction)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat("\ncvFraction was not specified, \n Using default value of 0.8 (cvFraction = 0.8)\n\n")
             cvFraction = 0.8
           }
           
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           index <- createDataPartition(Data[,classCol],p=cvFraction,times=1)
           trainData <- Data[1:nrow(Data) %in% index$Resample1,]
           testData <- Data[!(1:nrow(Data) %in% index$Resample1),]
           if(!silent) cat("Proportion of Test/Train Data was : ",nrow(testData)/nrow(trainData),"\n")
           obj <- data.frame(gamma=gamma,cost=cost)
           classificationResults <- do.call(genclassifier,c(list(trainData=trainData,testData=testData,predictorColNames=predictorColNames,
                                                  featureColNames=featureColNames,expand.grid(obj),...)))
           
           accTest <- classificationResults[["accTest"]]
           ConfMatrix <- classificationResults[["ConfMatrix"]]
           accTestRun <- NULL
           if(!silent){
             print(paste("Test holdout Accuracy is ",signif(accTest,2)))
             cat("holdout classification Analysis:",'\ncvFraction :', cvFraction,"\nTest Accuracy",signif(mean(accTest),2))
             
             cat("\n*Legend:\ncvFraction = Fraction of data to keep for training data",
                 "\nTest Accuracy = Accuracy from the Testing dataset\n")
           }
           
         },
         LOTO = {
           if(!silent) cat("\nPerforming Leave-one-Trial-out Cross-validation \n (Might take some time depending upon the size of dataset)  \n")
           
           if(missing(genclassifier)){
             # \n for separating the lines as issues with R CMD check - outputs notes
             if(!silent) cat("\ngenclassifier was not specified,\n Using default value of Classifier.svm.Folds (genclassifier = Classifier.svm)\n")
             genclassifier <- Classifier.svm
             
           }
           
           # get feature columns without response
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           
           index <- createFolds(Data[,classCol],k=nrow(Data),list=FALSE)
           obj <- data.frame(gamma=gamma,cost=cost)
           accTestRun <- vector()
           classificationResults <- list()
           ConfMatrix <- list()
           for(i in seq_along(index)){
             DataTrain <- Data[-i,]
             DataTest <-  Data[i,]
             
             classificationResults[[i]] <- do.call(genclassifier,c(list(trainData=DataTrain,testData=DataTest,predictorColNames=predictorColNames,
                                                                        featureColNames=featureColNames,expand.grid(obj),...)))
             accTestRun[i] <- classificationResults[[i]][["accTest"]]
             ConfMatrix[[i]] <- classificationResults[[i]][["ConfMatrix"]]
             
           }
           # no sense in getting an LDA model from a specific model
           accTest <- mean(accTestRun,na.rm = TRUE)
           
           
           if(!silent){
             print(paste("Test LOTO classification Accuracy is ",signif(accTest,2)))
             # print parameters used
             cat("leave-one-Trial-out classification Analysis:","\nTest Accuracy",signif(accTest,2))
             cat("\n*Legend:\nTest Accuracy = Mean accuracy from the Testing dataset\n")
           }
           
         }
  )
  #Results <- list(acc=acc,accTest=accTest)
  #return(Results)
  # get overall confusion Matrix results
  ConfusionMatrixResults <- overallConfusionMetrics(ConfMatrix)
  
  if(!is.null(NewData)){
    newDataprediction <- predictNewData(classificationResults$model,NewData,type='class')
    classificationResults <- list(classificationResults=classificationResults,newDataprediction=newDataprediction)
  }
  
  
  if(extendedResults){
    Results <- list(classificationResults = classificationResults,accTest = accTest,ConfMatrix=ConfMatrix,ConfusionMatrixResults=ConfusionMatrixResults,accTestRun = accTestRun)
    return(Results)
  }else return(accTest)
}

# get tuned parameters
getTunedParam <- function(tuneTrainData,classCol,classifierName,featureColNames,ranges=NULL,silent=FALSE){

  classifierFun <- get(classifierName)
  if(missing(featureColNames)) featureColNames <- 1:length(names(tuneTrainData))
  # defaults
  if(!silent) print('Begining Tuning Classifier')
  # only in case of svm, suggest
  if(classifierName=="svm"){
    if (is.null(ranges)) ranges = list(gamma = 2^(-1:1), cost = 2^(2:4))
    obj <- tune(classifierFun, train.y = tuneTrainData[,classCol],train.x = tuneTrainData[,featureColNames],
                ranges = ranges,tunecontrol = tune.control(sampling = "fix"))
  } else if(classifierName=="knn3") {
    # chgoose range of k
    if (is.null(ranges)) ranges = 1:10
    obj <- tune.knn(y = tuneTrainData[,classCol],x = tuneTrainData[,featureColNames],
                k = ranges,tunecontrol = tune.control(sampling = "fix"))
  }
  if(!silent) print(summary(obj))
  if(!silent) plot(obj)


  return(obj)

}

# classifier functions for knn3 and svm
Classifier.svm.Folds <- function(trainData,testData,ModelTestData,predictorColNames,featureColNames,...){
  model <- do.call('svm',c(list(y=trainData[,predictorColNames],x=trainData[,featureColNames]),...))
  # test with train data
  pred <- predict(model, testData[,featureColNames])
  acc <- sum(1 * (pred==testData[,predictorColNames]))/length(pred)
  predTest <- predict(model, ModelTestData[,featureColNames])
  accTest <- sum(1 * (predTest==ModelTestData[,predictorColNames]))/length(predTest)
  ConfMatrix <- confusionMatrix(ModelTestData[,predictorColNames],predTest)
  accList <- list(acc=acc,accTest=accTest,ConfMatrix=ConfMatrix)
  return(accList)

}

Classifier.knn.Folds <- function(trainData,testData,ModelTestData,predictorColNames,featureColNames,obj,...){
    model <- do.call("knn3",c(list(y=trainData[,predictorColNames],x=trainData[,featureColNames]),expand.grid(obj$best.parameters),...))
    # test with train data
    pred <- predict(model, testData[,featureColNames],type="class")
    acc <- sum(1 * (pred==testData[,predictorColNames]))/length(pred)
    predTest <- predict(model, ModelTestData[,featureColNames],type="class")
    accTest <- sum(1 * (predTest==ModelTestData[,predictorColNames]))/length(predTest)
    ConfMatrix <- confusionMatrix(ModelTestData[,predictorColNames],predTest)
    accList <- list(acc=acc,accTest=accTest,ConfMatrix=ConfMatrix)
    return(accList)
}


Classifier.svm <- function(trainData,testData,predictorColNames,featureColNames,...){
  model <- do.call('svm',c(list(y=trainData[,predictorColNames],x=trainData[,featureColNames]),...))
  # test with train data
  pred <- predict(model, testData[,featureColNames])
  accTest <- sum(1 * (pred==testData[,predictorColNames]))/length(pred)
  ConfMatrix <- confusionMatrix(testData[,predictorColNames],pred)
  accList <- list(accTest=accTest,ConfMatrix=ConfMatrix,model=model)
  return(accList)
  
}

Classifier.knn <- function(trainData,testData,predictorColNames,featureColNames,obj,...){
  model <- do.call("knn3",c(list(y=trainData[,predictorColNames],x=trainData[,featureColNames]),expand.grid(obj$best.parameters),...))
  # test with train data
  pred <- predict(model, testData[,featureColNames],type="class")
  accTest <- sum(1 * (pred==testData[,predictorColNames]))/length(pred)
  ConfMatrix <- confusionMatrix(testData[,predictorColNames],pred)
  accList <- list(accTest=accTest,ConfMatrix=ConfMatrix,model=model)
  return(accList)
}
