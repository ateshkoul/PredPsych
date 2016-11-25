#' Generic Classification and Regression Tree Function
#' 
#'
#' A simple function to create Classification and Regression Trees
#' 
#' @param Data         (dataframe) a data frame with regressors and response
#' @param classCol  (numeric) which column should be used as response col
#' @param selectedCols  (optional)(numeric) which columns should be treated as data(features + response) (defaults to all columns)
#' @param tree         which cart model to implement; One of the following values:
#'      \itemize{
#'      \item CART     =   Classification And Regress Tree; 
#'      \item CARTNAHF = Crossvalidated Half Model Tree removing missing values;
#'      \item CARTHF =   Crossvalidated Half Model Tree With missing values;
#'      \item CF =   Conditional inference framework Tree;
#'      \item RF =   Random Forest Tree;    
#'      }
#' @details 
#' The function implements the CaRT modelling. DTmodels fall under the general ‘Tree based methods’ 
#' involving generation of a recursive binary tree (Hastie et al., 2009).
#' In terms of input, DT  models can handle both continuous and categorical variables
#' as well as missing data. From the input data, DT  models build a set of logical ‘if ..then’ rules
#' that permit accurate prediction of the input cases.
#' 
#' Unlike regression methods like GLMs,  Decision Trees are more flexible and can model nonlinear interactions.           
#' 
#' @return  model result for the input tree \code{Results}  
#' @examples
#' # generate a cart model for 10% of the data with cross-validation
#' model <- DTModel(Data = KinData[,c(1,2,12,22,32,42,52,62,72,82,92,102,112)],classCol=1,tree='CARTHF')
#' 
#' 
#' 
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#' @export
DTModel <- function(Data,classCol,selectedCols,tree,...){

  
  
  
  library(caret)

  # if nothing specific is provided, default to all the columns
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  # get the features
  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
  responseColName <- names(Data)[classCol]

  # make it a factor anyways
  Data[,classCol] <- factor(Data[,classCol])
  
  
  switch(tree,
         CART = {
           library(rpart)
           print("Generating Full Model Tree")
           # Full tree
           modelF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=Data[,selectedCols],method = 'class')  
           #summary(modelF)
           plotcp(modelF)
           # plot tree
           print(plot(modelF, uniform=TRUE,main="Classification Tree"))
           print(text(modelF, use.n=TRUE, all=TRUE, cex=.8))
           print(modelF)
           print('done')
           return(modelF)},
         
         CARTNAHF = {
             library(rpart)
             print("Generating crossvalidated Half Model Tree NO Mssing values")
             # remove NAs as I use a stratified cross validation (may not be necessary)
             DatNoNA <- Data[!is.na(Data[,classCol]),]
             # Just to be sure that the response is a factor for classification
             DatNoNA[,classCol] <- factor(DatNoNA[,classCol])


              # just divide as test and train if u want
              k = 2
              # use stratified cross validation instead
              # use 50% data for training
              set.seed(111)
              trainIndex <- createFolds(DatNoNA[,classCol],list = FALSE,k=k)
              train <- DatNoNA[trainIndex==1,]
              test <- DatNoNA[trainIndex==2,]
              modelNAHF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=train[,selectedCols],method = 'class')
              preDicNAHF <- predict(modelNAHF,test[,featureColNames],type='vector')
              accNAHF <- sum(1 * (preDicNAHF==test[,classCol]))/length(preDicNAHF)
              
              
              cp = modelNAHF$cptable[which( modelNAHF$cptable[,'xerror']==min(modelNAHF$cptable[,'xerror'])),'CP']
              prunedModelNAHF <- prune(modelNAHF,cp=cp)
              
              plot(prunedModelNAHF, uniform=TRUE,
                   main="Pruned Classification Tree HF (without Missing)")
              text(prunedModelNAHF, use.n=TRUE, all=TRUE, cex=.8)
              print(prunedModelNAHF)
              
              #summary(modelHF)
              # plot(modelHF, uniform=TRUE,
              #      main="Classification Tree HF")
              # text(modelHF, use.n=TRUE, all=TRUE, cex=.8)
              # print(modelHF)
              print(paste0("The accuracy of the model was ",signif(accNAHF,2)))
              print('done')
              return(prunedModelNAHF)},
         CARTHF = {
           library(rpart)
           print("Generating crossvalidated Half Model Tree With Missing Values")
           # just divide as test and train if u want
            k = 2
            set.seed(111)
            trainIndex <- createFolds(Data[,classCol],list = FALSE,k=k)
            train <- Data[trainIndex==1,]
            test <- Data[trainIndex==2,]
            modelHF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=train[,selectedCols],method = 'class')
            preDicHF <- predict(modelHF,test[,selectedCols],type='vector')
            accHF <- sum(1 * (preDicHF==test[,classCol]))/length(preDicHF)
            
            #prune the model for the one with min crossvalidation error
            cp = modelHF$cptable[which(modelHF$cptable[,'xerror']==min(modelHF$cptable[,'xerror'])),'CP']
            prunedModelHF <- prune(modelHF,cp=cp)
            
            plot(prunedModelHF, uniform=TRUE,
                 main="Pruned Classification Tree HF")
            text(prunedModelHF, use.n=TRUE, all=TRUE, cex=.8)
            print(prunedModelHF)
            
            #summary(modelHF)
            # plot(modelHF, uniform=TRUE,
            #      main="Classification Tree HF")
            # text(modelHF, use.n=TRUE, all=TRUE, cex=.8)
            # print(modelHF)
            print(paste0("The accuracy of the model was ",signif(accHF,2)))
            print('done')
            return(prunedModelHF)},
         CF = {# Cluster tree
           library(party)
           print("Generating conditional inference framework Tree")
           # remove NAs as I use a stratified cross validation (may not be necessary)
           DatNoNA <- Data[!is.na(Data[,classCol]),]
           # Just to be sure that the response is a factor for classification
           DatNoNA[,classCol] <- factor(DatNoNA[,classCol])
           modelCF <- ctree(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCols])
           #summary(modelCF)
           print(plot(modelCF))
           print(modelCF)
           print('done')
           return(modelCF)},
         RF = {  # Random forest
           library(randomForest)
            print("Generating Random Forest Tree")
           # remove NAs as I use a stratified cross validation (may not be necessary)
           DatNoNA <- Data[!is.na(Data[,classCol]),]
           # Just to be sure that the response is a factor for classification
           DatNoNA[,classCol] <- factor(DatNoNA[,classCol])
           modelRF <- randomForest(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCols])
           print(modelRF) # view results
           # importance of each predictor
           print(importance(modelRF)) 
           print('done')
           return(modelRF)}
  )
}
