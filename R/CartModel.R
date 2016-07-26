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
#'          
#' @return  Cart model result for the input tree \code{Results}  
#'
#'@author
#'Atesh Koul, C'MON group, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
CartModel <- function(Data,responseCol,selectedCol,tree,...){

  library(rpart)
  library(party)
  library(randomForest)
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
         modelF = {print("Generating Full Model Tree")
           # Full tree
           modelF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=Data[,selectedCol],method = 'class')  
           #summary(modelF)
           plotcp(modelF)
           # plot tree
           print(plot(modelF, uniform=TRUE,main="Classification Tree"))
           print(text(modelF, use.n=TRUE, all=TRUE, cex=.8))
           return(modelF)},
         
           modelNAHF = {print("Generating crossvalidated Half Model Tree NO NA")
             # remove NAs as I use a stratified cross validation (may not be necessary)
             DatNoNA <- Data[!is.na(Data[,responseCol]),]
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
              #summary(modelNAHF)
              print(plot(modelNAHF, uniform=TRUE,
                         main="Classification Tree HF"))
              print(text(modelNAHF, use.n=TRUE, all=TRUE, cex=.8))
              return(modelNAHF)},
         modelHF = {print("Generating crossvalidated Half Model Tree With NA")
           # just divide as test and train if u want
            k = 2
            trainIndex <- createFolds(DatNoNA[,responseCol],list = FALSE,k=k)
            trainX <- Data[trainIndex==1,]
            testX <- Data[!trainIndex==2,]
            modelHF <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=trainX[,selectedCol],method = 'class')
            preDicHF <- predict(modelHF,testX,type='matrix')
            #summary(modelHF)
            return(modelHF)},
         modelCF = {# Cluster tree
           print("Generating conditional inference framework Tree")
            modelCF <- ctree(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCol])
            #summary(modelCF)
            print(plot(modelCF))
            return(modelCF)},
         modelRF = {  # Random forest
            print("Generating Random Forest Tree")
            modelRF <- randomForest(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCol])
            print(modelRF) # view results
            # importance of each predictor
            print(importance(modelRF)) 
            return(modelRF)}
  )
}
