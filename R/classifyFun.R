#'simple function to create permutation testing of a classifier
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item classifierName(string) = name of the classifier to be used
#'      \item genclassifier(function or string) = a classifier function or a name (e.g. Classifier.svm)
#'      \item Data(dataframe) = dataframe of the data
#'      \item predictorCol(numeric) = column number that contains the variable to be predicted
#'      \item selectedCols(numeric) = all the columns of data that would be used either as predictor or as feature
#'      \item ranges(list) = ranges for tuning support vector machine
#'      \item tune(logical) = whether tuning of svm parameters should be performed or not
#'      \item cost(numeric) = regularization parameter of svm
#'      \item gamma(numeric) = rbf kernel parameter
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item acc = crossvalidation accuracy
##'     \item accTest = Test accuracy
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
classifyFun <- function(classifierName,genclassifier,Data,predictorCol,selectedCols,ranges=NULL,tune=FALSE,cost=1,gamma=0.5,...){
  # a simplistic k-fold crossvalidation
  # For cross validation
  library(e1071)
  library(caret)
  # dont use a constant set.seed with permutation testing
  # u will get a constant accuracy!!
  set.seed(123)

  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))

    # get the features
  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-match(names(Data)[predictorCol],selectedColNames)]
  predictorColNames <- names(Data)[predictorCol]

  Data = Data[,selectedCols]
  Data[,predictorColNames] = factor(Data[,predictorColNames])

  # if predictor has missing, remove those columns
  if(sum(is.na(Data[,predictorColNames]))>0) Data <- Data[!is.na(Data[,predictorColNames]),]

  if(tune) {
    folds = 3
    # use stratified cross validation instead
    # divide the data into 3 parts:
    # 1. for tuning parameters
    # 2. for training model
    # 3. for testing prediction (on a data that it has never seen ever)
    #
    # Keeping more for tuning
    trainIndexOverall <- createFolds(Data[,predictorCol],list = FALSE,k = folds)
    # leave first part for tuning the classifier
    tuneTrainData <- Data[trainIndexOverall==1,]
    ModelTrainData <- Data[trainIndexOverall==2,]
    ModelTestData <- Data[trainIndexOverall==3,]
    obj <- getTunedParam(tuneTrainData,predictorCol,classifierName,featureColNames,ranges)["best.parameters"]

  } else{
    folds = 2
    trainIndexOverall <- createFolds(Data[,predictorColNames],list = FALSE,k = folds)
    ModelTrainData <- Data[trainIndexOverall==1,]
    ModelTestData <- Data[trainIndexOverall==2,]
    obj <- data.frame(gamma=gamma,cost=cost)
  }

  kFold <- 10
  #initialising vectors
  acc <- rep(NA,kFold)
  accTest <- rep(NA,kFold)

  trainIndexModel <- createFolds(ModelTrainData[,predictorColNames],list = FALSE,k = kFold)

  print('Begining k-fold Classification')
  for (i in 1:kFold){
    trainDataFold <- ModelTrainData[!trainIndexModel==i,]
    testDataFold <- ModelTrainData[trainIndexModel==i,]

    # the classifier has generic
    # generic error function:
    acc[i] = do.call(genclassifier,c(list(trainData=trainDataFold,testData=testDataFold,ModelTestData=ModelTestData,predictorColNames=predictorColNames,
                                             featureColNames=featureColNames,expand.grid(obj),...)))[["acc"]]
    accTest[i] = do.call(genclassifier,c(list(trainData=trainDataFold,testData=testDataFold,ModelTestData=ModelTestData,predictorColNames=predictorColNames,
                                                 featureColNames=featureColNames,expand.grid(obj),...)))[["accTest"]]
  }

  print(paste("Mean CV Accuracy",mean(acc)))
  print(paste("Mean Test Accuracy",mean(accTest)))


Results <- list(acc=acc,accTest=accTest)
return(Results)

}

# get tuned parameters
getTunedParam <- function(tuneTrainData,predictorCol,classifierName,featureColNames,ranges=NULL){

  classifierFun <- get(classifierName)
  if(missing(featureColNames)) featureColNames <- 1:length(names(tuneTrainData))
  # defaults
  print('Begining Tuning Classifier')
  # only in case of svm, suggest
  if(classifierName=="svm"){
    if (is.null(ranges)) ranges = list(gamma = 2^(-1:1), cost = 2^(2:4))
    obj <- tune(classifierFun, train.y = tuneTrainData[,predictorCol],train.x = tuneTrainData[,featureColNames],
                ranges = ranges,tunecontrol = tune.control(sampling = "fix"))
  } else if(classifierName=="knn3") {
    # chgoose range of k
    if (is.null(ranges)) ranges = 1:10
    obj <- tune.knn(y = tuneTrainData[,predictorCol],x = tuneTrainData[,featureColNames],
                k = ranges,tunecontrol = tune.control(sampling = "fix"))
  }
  print(summary(obj))
  plot(obj)


  return(obj)

}

# classifier functions for knn3 and svm
Classifier.svm <- function(trainData,testData,ModelTestData,predictorColNames,featureColNames,...){
  model <- do.call('svm',c(list(y=trainData[,predictorColNames],x=trainData[,featureColNames]),...))
  # test with train data
  pred <- predict(model, testData[,featureColNames])
  acc <- sum(1 * (pred==testData[,predictorColNames]))/length(pred)
  predTest <- predict(model, ModelTestData[,featureColNames])
  accTest <- sum(1 * (predTest==ModelTestData[,predictorColNames]))/length(predTest)
  accList <- list(acc=acc,accTest=accTest)
  return(accList)

}

Classifier.knn <- function(trainData,testData,ModelTestData,predictorColNames,featureColNames,obj,...){
    model <- do.call("knn3",c(list(y=trainData[,predictorColNames],x=trainData[,featureColNames]),expand.grid(obj$best.parameters),...))
    # test with train data
    pred <- predict(model, testData[,featureColNames],type="class")
    acc <- sum(1 * (pred==testData[,predictorColNames]))/length(pred)
    predTest <- predict(model, ModelTestData[,featureColNames],type="class")
    accTest <- sum(1 * (predTest==ModelTestData[,predictorColNames]))/length(predTest)
    accList <- list(acc=acc,accTest=accTest)
    return(accList)
}











