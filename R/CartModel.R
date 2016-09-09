#' Generic Classification and Regression Tree Function
#' 
#'
#' A simple function to create Classification and Regression Trees
#' 
#' @param Data         (dataframe) a data frame with regressors and response
#' @param responseCol  (numeric) which column should be used as response col
#' @param selectedCol  (optional)(numeric) which columns should be treated as data(features + response) (defaults to all columns)
#' @param tree         which cart model to implement; One of the following values:
#'      \itemize{
#'      \item modelF  =   Full Model Tree; 
#'      \item modelNAHF = Crossvalidated Half Model Tree removing missing values;
#'      \item modelHF =   Crossvalidated Half Model Tree With missing values;
#'      \item modelCF =   Conditional inference framework Tree;
#'      \item modelRF =   Random Forest Tree;    
#'      }
#' @details 
#' The function implements the CaRT modelling. CaRT models fall under the general 'Tree based methods' involving generation of a recursive binary tree 
#' (Hastie et al., 2009). In terms of input, Cart models can handle both continuous and categorical variables 
#' as well as missing data. From the input data, Cart models build a set of logical 'if ..then' rules that permit
#' accurate prediction of the input cases. 
#' 
#' Unlike regression methods like GLMs,  CaRT models are more flexible and can model nonlinear interactions.           
#' 
#' @return  Cart model result for the input tree \code{Results}  
#' @examples
#' # generate a cart model for 10% of the data with cross-validation
#' model <- CartModel(Data = KinData[,c(1,2,12,22,32,42,52,62,72,82,92,102,112)],responseCol=1,tree='modelHF')
#' 
#' 
#' 
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#' @export
CartModel <- function(Data,responseCol,selectedCol,tree,...){

  
  
  
  library(caret)

  # if nothing specific is provided, default to all the columns
  if(missing(selectedCol))  selectedCol <- 1:length(names(Data))
  # get the features
  selectedColNames <- names(Data)[selectedCol]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(Data)[responseCol],selectedColNames)]
  responseColName <- names(Data)[responseCol]

  # make it a factor anyways
  Data[,responseCol] <- factor(Data[,responseCol])
  
  
  switch(tree,
         modelF = {
           library(rpart)
           print("Generating Full Model Tree")
           # Full tree
           modelF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=Data[,selectedCol],method = 'class')  
           #summary(modelF)
           plotcp(modelF)
           # plot tree
           print(plot(modelF, uniform=TRUE,main="Classification Tree"))
           print(text(modelF, use.n=TRUE, all=TRUE, cex=.8))
           print(modelF)
           print('done')
           return(modelF)},
         
           modelNAHF = {
             library(rpart)
             print("Generating crossvalidated Half Model Tree NO Mssing values")
             # remove NAs as I use a stratified cross validation (may not be necessary)
             DatNoNA <- Data[!is.na(Data[,responseCol]),]
             # Just to be sure that the response is a factor for classification
             DatNoNA[,responseCol] <- factor(DatNoNA[,responseCol])


              # just divide as test and train if u want
              k = 2
              # use stratified cross validation instead
              # use 50% data for training
              set.seed(111)
              trainIndex <- createFolds(DatNoNA[,responseCol],list = FALSE,k=k)
              train <- DatNoNA[trainIndex==1,]
              test <- DatNoNA[trainIndex==2,]
              modelNAHF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=train[,selectedCol],method = 'class')
              preDicNAHF <- predict(modelNAHF,test[,featureColNames],type='vector')
              accNAHF <- sum(1 * (preDicNAHF==test[,responseCol]))/length(preDicNAHF)
              #summary(modelNAHF)
              plot(modelNAHF, uniform=TRUE,
                         main="Classification Tree HF (without Missing)")
              text(modelNAHF, use.n=TRUE, all=TRUE, cex=.8)
              print(modelNAHF)
              print(paste0("The accuracy of the model was ",signif(accNAHF,2)))
              print('done')
              return(modelNAHF)},
         modelHF = {
           library(rpart)
           print("Generating crossvalidated Half Model Tree With Missing Values")
           # just divide as test and train if u want
            k = 2
            set.seed(111)
            trainIndex <- createFolds(Data[,responseCol],list = FALSE,k=k)
            train <- Data[trainIndex==1,]
            test <- Data[trainIndex==2,]
            modelHF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=train[,selectedCol],method = 'class')
            preDicHF <- predict(modelHF,test[,selectedCol],type='vector')
            accHF <- sum(1 * (preDicHF==test[,responseCol]))/length(preDicHF)
            #summary(modelHF)
            plot(modelHF, uniform=TRUE,
                 main="Classification Tree HF")
            text(modelHF, use.n=TRUE, all=TRUE, cex=.8)
            print(modelHF)
            print(paste0("The accuracy of the model was ",signif(accHF,2)))
            print('done')
            return(modelHF)},
         modelCF = {# Cluster tree
           library(party)
           print("Generating conditional inference framework Tree")
           # remove NAs as I use a stratified cross validation (may not be necessary)
           DatNoNA <- Data[!is.na(Data[,responseCol]),]
           # Just to be sure that the response is a factor for classification
           DatNoNA[,responseCol] <- factor(DatNoNA[,responseCol])
           modelCF <- ctree(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCol])
           #summary(modelCF)
           print(plot(modelCF))
           print(modelCF)
           print('done')
           return(modelCF)},
         modelRF = {  # Random forest
           library(randomForest)
            print("Generating Random Forest Tree")
           # remove NAs as I use a stratified cross validation (may not be necessary)
           DatNoNA <- Data[!is.na(Data[,responseCol]),]
           # Just to be sure that the response is a factor for classification
           DatNoNA[,responseCol] <- factor(DatNoNA[,responseCol])
           modelRF <- randomForest(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCol])
           print(modelRF) # view results
           # importance of each predictor
           print(importance(modelRF)) 
           print('done')
           return(modelRF)}
  )
}
