#'simple function to create Classification and Regression tree of Data
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item DataFrame = a data frame with regressors and response
#'      \item responseCol = which column should be used as response col
#'      \item selectedCol (optional) = which columns should be treated as data(features + response) (defaults to all columns)
#'
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item Results = list containing Cart models
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
CartModel <- function(DataFrame,responseCol,selectedCol,...){

  library(rpart)
  library(party)
  library(randomForest)
  library(caret)

  if(missing(responseCol)) responseCol <- grep("Risposta",names(DataFrame))

  # if nothing specific is provided, default to all the columns
  if(missing(selectedCol))  selectedCol <- 1:length(names(DataFrame))
  # get the features
  selectedColNames <- names(DataFrame)[selectedCol]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(DataFrame)[responseCol],selectedColNames)]
  responseColName <- names(DataFrame)[responseCol]

  # make it a factor anyways
  DataFrame[,responseCol] <- factor(DataFrame[,responseCol])


  print("Generating Full Model Tree")
  # Full tree
  modelF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DataFrame[,selectedCol],method = 'class')
  summary(modelF)
  plotcp(modelF)
  # plot tree
  print(plot(modelF, uniform=TRUE,
             main="Classification Tree"))
  print(text(modelF, use.n=TRUE, all=TRUE, cex=.8))


  print("Generating crossvalidated Half Model Tree NO NA")
  # remove NAs as I use a stratified cross validation (may not be necessary)
  DatNoNA <- DataFrame[!is.na(DataFrame[,responseCol]),]
  # Just to be sure that the response is a factor for classification
  DatNoNA[,responseCol] <- factor(DatNoNA[,responseCol])

  # optional scaling
  # takes care if u input negative values also
  # usedColNames <- names(Data)[selectedCol]
  # ActualselectedCol <- usedColNames[-grep(names(Data)[responseCol],usedColNames)]
  # DatNoNA[,ActualselectedCol] <- scale(DatNoNA[,ActualselectedCol])

  # just divide as test and train if u want
  k = 2
  # use stratified cross validation instead
  # use 50% data for training
  trainIndex <- createFolds(DatNoNA[,responseCol],list = FALSE,k=k)
  trainX <- DatNoNA[trainIndex==1,]
  testX <- DatNoNA[!trainIndex==2,]
  modelNAHF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=trainX[,selectedCol],method = 'class')
  preDicNAHF <- predict(modelNAHF,testX,type='matrix')
  summary(modelNAHF)
  print(plot(modelNAHF, uniform=TRUE,
             main="Classification Tree HF"))
  print(text(modelNAHF, use.n=TRUE, all=TRUE, cex=.8))

  print("Generating crossvalidated Half Model Tree With NA")

  # just divide as test and train if u want
  k = 2
  trainIndex <- createFolds(DatNoNA[,responseCol],list = FALSE,k=k)
  trainX <- DataFrame[trainIndex==1,]
  testX <- DataFrame[!trainIndex==2,]
  modelHF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=trainX[,selectedCol],method = 'class')
  preDicHF <- predict(modelHF,testX,type='matrix')
  summary(modelHF)


  # Cluster tree
  print("Generating CF Tree")
  modelCF <- ctree(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCol])
  summary(modelCF)
  print(plot(modelCF))


  # Random forest
  print("Generating Random Forest")
  modelRF <- randomForest(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCol])
  print(modelRF) # view results
  print(importance(modelRF)) # importance of each predictor

  Result <- list(modelRF=modelRF,modelCF=modelCF,modelF=modelF,modelHF=modelHF,modelNAHF=modelNAHF,preDicNAHF=preDicNAHF,preDicHF=preDicHF)
  return(Result)

}
