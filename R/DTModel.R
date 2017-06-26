#' Generic Decision Tree Function
#' 
#'
#' A simple function to create Decision Trees
#' 
#' @param Data                (dataframe) a data frame with regressors and response
#' @param classCol            (numeric) which column should be used as response col
#' @param selectedCols        (optional)(numeric) which columns should be treated as data(features + response) (defaults to all columns)
#' @param tree                           which decision tree model to implement; One of the following values:
#'      \itemize{
#'      \item CART        =   Classification And Regression Tree; 
#'      \item CARTNACV    =   Crossvalidated CART Tree removing missing values;
#'      \item CARTCV      =   Crossvalidated CART Tree With missing values;
#'      \item CF          =   Conditional inference framework Tree;
#'      \item RF          =   Random Forest Tree;    
#'      }
#' @param cvType              (optional) (string) which type of cross-validation scheme to follow - only in case of CARTCV or CARTNACV;
#'                            One of the following values:
#'      \itemize{
#'      \item folds       =   k-fold cross-validation 
#'      \item LOSO        =   Leave-one-subject-out cross-validation
#'      \item holdout     =   (default) holdout Crossvalidation. Only a portion of data (cvFraction) is used.
#'      \item LOTO        =   Leave-one-trial out cross-validation.
#'      }
#' @param ntrainTestFolds     (optional) (parameter for only k-fold cross-validation) No. of folds for training and testing dataset
#' @param nTrainFolds         (optional) (parameter for only k-fold cross-validation) No. of folds in which to further divide Training dataset
#' @param modelTrainFolds =   (optional) (parameter for only k-fold cross-validation) specific folds from the first train/test split
#'  (ntrainTestFolds) to use for training 
#' @param foldSep             (numeric)  (parameter for only Leave-One_subject Out) mandatory column number for Leave-one-subject out cross-validation.
#' @param cvFraction          (optional) (numeric) Fraction of data to keep for training data
#' @param extendedResults     (optional) (logical) Return extended results with model and other metrics
#' @param SetSeed             (optional) (logical) Whether to setseed or not. use SetSeed to seed the random number generator to get consistent results; 
#' @param silent              (optional) (logical) whether to print messages or not
#' @param NewData             (optional) (dataframe) New Data frame features for which the class membership is requested  
#' @param ...                 (optional) additional arguments for the function
#' 
#' @details 
#' The function implements the Decision Tree models (DT models).
#' DT models fall under the general "Tree based methods"
#' involving generation of a recursive binary tree (Hastie et al., 2009).
#' In terms of input, DT  models can handle both continuous and categorical variables
#' as well as missing data. From the input data, DT  models build a set of logical "if ..then" rules
#' that permit accurate prediction of the input cases.
#' 
#' The function "rpart" handles the missing data by creating surrogate variables 
#' instead of removing them entirely (Therneau, & Atkinson, 1997). 
#' This could be useful in case the data contains multiple missing values. 
#' 
#' Unlike regression methods like GLMs,  Decision Trees are more flexible and can model nonlinear interactions.           
#' 
#' @return  model result for the input tree \code{Results}  or Test accuracy \code{accTest}. If \code{extendedResults} = TRUE 
#' outputs Test accuracy \code{accTest} of discrimination,\code{ConfMatrix} Confusion matrices and \code{fit} the model 
#' and  \code{ConfusionMatrixResults} Overall cross-validated confusion matrix results  
#' 
#' @examples
#' # generate a cart model for 10% of the data with cross-validation
#' model <- DTModel(Data = KinData,classCol=1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112), tree='CART')
#' # Output:
#' # Performing Decision Tree Analysis 
#' #
#' # Generating Model Tree
#' #
#' # Performing holdout Cross-validation
#' # 
#' # cvFraction was not specified,
#' #  Using default value of 0.8 (cvFraction = 0.8)"
#' # 
#' # Proportion of Test/Train Data was :  0.2488954 
#' # --CART MOdel --
#' # 
#' # [1] "Test holdout Accuracy is  0.69"
#' # holdout CART Analysis: 
#' # cvFraction : 0.8 
#' # Test Accuracy 0.69
#' # *Legend:
#' # cvFraction = Fraction of data to keep for training data 
#' # Test Accuracy = Accuracy from the Testing dataset
#' # [1] "done"
#' 
#' # Alternate uses:  
#' # holdout cross-validation with removing missing values
#' model <- DTModel(Data = KinData,classCol=1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' tree='CARTNACV')
#' 
#' # k-fold cross-validation with removing missing values
#' model <- DTModel(Data = KinData,classCol=1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' tree='CARTNACV',cvType="folds")
#' 
#' # holdout cross-validation without removing missing values
#' model <- DTModel(Data = KinData,classCol=1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' tree='CARTCV')
#' 
#' # k-fold cross-validation without removing missing values
#' model <- DTModel(Data = KinData,classCol=1,
#' selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112),
#' tree='CARTCV',cvType="folds")
#' 
#' @import caret rpart party
#' @importFrom randomForest randomForest importance
#' @importFrom graphics plot text
#' @author
#' Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#' \email{atesh.koul@@iit.it}
#' 
#' @references 
#' Hastie, T., Tibshirani, R., & Friedman, J. (2009). The Elements of Statistical Learning. 
#' Springer Series in Statistics (2nd ed., Vol. 1). New York, NY: Springer New York.
#' 
#' Terry Therneau, Beth Atkinson and Brian Ripley (2015). rpart: Recursive Partitioning and Regression Trees. 
#' R package version 4.1-10.  https://CRAN.R-project.org/package=rpart
#' 
#' Therneau, T. M., & Atkinson, E. J. (1997). An introduction to recursive partitioning using the RPART routines (Vol. 61, p. 452).
#' Mayo Foundation: Technical report.
#' 
#' @export
DTModel <- function(Data,classCol,selectedCols,tree,cvType,nTrainFolds,ntrainTestFolds,modelTrainFolds,foldSep,cvFraction,
                    extendedResults = FALSE,SetSeed=TRUE,silent=FALSE,NewData=NULL,...){

  #library(caret)
  if(!silent) cat("\nPerforming Decision Tree Analysis \n\n")
  
  if(SetSeed)  set.seed(111)
  
  if(!(tree %in% c("CART","CARTNACV","CARTCV","CF","RF"))) stop("Unknown tree provided")
  
  switch(tree,
         CART = {
           #library(rpart)
           selectedColNames <- names(Data)[selectedCols]
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           responseColName <- names(Data)[classCol]
           
           if(!missing(cvType)) stop("cvType provided is different from holdout. Did you want to perform crossvalidation? 
            Please Use tree = CARTNACV or tree = CARTCV for Cross-validated CART")
           
           
           
           if(!silent) cat("Generating Model Tree\n")
           
           if(!silent) cat("\nPerforming holdout Cross-validation\n")
           
           if(missing(cvFraction)){
             if(!silent) cat("\ncvFraction was not specified, \n Using default value of 0.8 (cvFraction = 0.8)\n")
             cvFraction = 0.8
           }
           
           prunIndex <- createDataPartition(y=Data[,classCol],p=cvFraction)
           DataTest <- Data[!(1:nrow(Data) %in% prunIndex$Resample1),]
           DataModel <- Data[1:nrow(Data) %in% prunIndex$Resample1,]
           
           cat("\nProportion of Test/Train Data was : ",nrow(DataTest)/nrow(DataModel),"\n")
           
           # Full tree
           fit <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DataModel[,selectedCols],method = 'class')  
           #summary(fit)
           cp = fit$cptable[which( fit$cptable[,'xerror']==min(fit$cptable[,'xerror'])),'CP']
           prunedModelF <- prune(fit,cp=cp)
           
           plot(prunedModelF, uniform=TRUE,
                main="Pruned Classification Tree")
           text(prunedModelF, use.n=TRUE, all=TRUE, cex=.8)
           if(!silent) print(prunedModelF)
           
           predictedTest <- predict(prunedModelF,DataTest[,featureColNames],type='class')
           truthTest <- DataTest[,classCol]
           accTest <- sum(predictedTest==truthTest)/length(predictedTest)
           
           ConfMatrix <- confusionMatrix(truthTest,predictedTest)
           
           if(!silent){
             print(paste("Test holdout Accuracy is ",signif(accTest,2)))
             cat("holdout CART Analysis:",'\ncvFraction :', cvFraction,"\nTest Accuracy",signif(mean(accTest),2))
             
             cat("\n*Legend:\ncvFraction = Fraction of data to keep for training data",
                 "\nTest Accuracy = Accuracy from the Testing dataset\n")
           }
           
           if(!is.null(NewData)){
             newDataprediction <- predictNewData(fit,NewData,type='class')
             fit <- list(fit=fit,newDataprediction=newDataprediction)
           }
           
           if(extendedResults){
             Results <- list(fit=fit,accTest=accTest,ConfMatrix=ConfMatrix)
             return(Results)
           }else return(accTest)
           
           if(!silent) print('done')
           },
         
         CARTNACV = {
           #library(rpart)
           if(!silent) print("Generating crossvalidated Tree NO Missing values")
           # remove NAs as I use a stratified cross validation (may not be necessary)
           DatNoNA <- Data[!is.na(Data[,classCol]),]
           # Just to be sure that the response is a factor for classification
           DatNoNA[,classCol] <- factor(DatNoNA[,classCol])
           
           if(missing(cvType)){
             if(!silent) cat("cvType was not specified, \n Using default holdout Cross-validation \n")
             cvType <- "holdout"
           }

           Results <- cv.CART(DatNoNA,classCol,selectedCols,cvType=cvType,ntrainTestFolds=ntrainTestFolds,
                              modelTrainFolds=modelTrainFolds,nTrainFolds=nTrainFolds,foldSep=foldSep,
                              extendedResults=extendedResults,silent=silent,NewData=NewData,...)
           
           
           if(!silent) print('done')
           return(Results)},
         CARTCV = {
           #library(rpart)
           if(!silent) print("Generating crossvalidated Tree With Missing Values")
           if(missing(cvType)){
             if(!silent) cat("cvType was not specified, \n Using default holdout Cross-validation \n")
             cvType <- "holdout"
           }
           # Important to write arguments otherwise the function doesn't receive the specific arguments
           # using the '=' for argument ensures that correct arguments are passed to the function
           Results <- cv.CART(Data,classCol,selectedCols,cvType=cvType,ntrainTestFolds=ntrainTestFolds,
                              modelTrainFolds=modelTrainFolds,nTrainFolds=nTrainFolds,foldSep=foldSep,
                              extendedResults=extendedResults,silent=silent,NewData=NewData,...)
           
           if(!silent) print('done')
           
           return(Results)
         },
         CF = {# Cluster tree
           #library(party)
           selectedColNames <- names(Data)[selectedCols]
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           responseColName <- names(Data)[classCol]
           if(!silent) print("Generating conditional inference framework Tree")
           # remove NAs as I use a stratified cross validation (may not be necessary)
           DatNoNA <- Data[!is.na(Data[,classCol]),]
           # Just to be sure that the response is a factor for classification
           DatNoNA[,classCol] <- factor(DatNoNA[,classCol])
           fit <- ctree(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCols])
           #summary(fit)
           if(!silent) print(plot(fit))
           if(!silent) print(fit)
           if(!silent) print('done')
           return(fit)},
         RF = {  # Random forest
           #library(randomForest)
           selectedColNames <- names(Data)[selectedCols]
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           responseColName <- names(Data)[classCol]
           if(!silent) print("Generating Random Forest Tree")
           # remove NAs as I use a stratified cross validation (may not be necessary)
           DatNoNA <- Data[!is.na(Data[,classCol]),]
           # Just to be sure that the response is a factor for classification
           DatNoNA[,classCol] <- factor(DatNoNA[,classCol])
           fit <- randomForest(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=DatNoNA[,selectedCols])
           if(!silent) print(fit) # view results
           # importance of each predictor
           if(!silent) print(importance(fit)) 
           if(!silent) print('done')
           return(fit)}
  )
}

cv.CART <- function(Data,classCol,selectedCols,cvType,ntrainTestFolds,modelTrainFolds,nTrainFolds,
                    foldSep,cvFraction,extendedResults = FALSE,silent=FALSE,NewData=NULL,...){
  # if nothing specific is provided, default to all the columns
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  
  if(!(cvType %in% c("holdout","folds","LOSO","LOTO"))) stop(cat("\n cvType is not one of holdout,folds or LOSO. You provided",cvType))
  
  # get the features
  selectedColNames <- names(Data)[selectedCols]
  responseColName <- names(Data)[classCol]
  
  # make it a factor anyways
  Data[,classCol] <- factor(Data[,classCol])
  switch(cvType,
         folds = {
           if(!silent) cat("\nPerforming k-fold Cross-validation \n\n")
           
           
           if(missing(ntrainTestFolds)){
             if(!silent) cat(paste0("\nntrainTestFolds was not specified, \n Using default value of 10 (ntrainTestFolds = 10)"))
             ntrainTestFolds <- 10
           }
           
           if(missing(nTrainFolds)){
             if(!silent) cat(paste0("\nnTrainFolds was not specified, \n Using default value of 10 fold cross-validation (nTrainFolds = 10)\n"))
             nTrainFolds <- 10
           }
           if(missing(modelTrainFolds)){
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
           FoldaccTest <- rep(NA,nTrainFolds)
           ConfMatrix <- list()
           for (i in 1:nTrainFolds){
             trainDataFold <- ModelTrainData[trainIndexModel!=i,]
             testDataFold <- ModelTrainData[trainIndexModel==i,]
             
             # CV = FALSE as we don't want leave-one-trial out cv
             fit <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=trainDataFold[,selectedCols],method = 'class')
             
             # The predict funcion redirects based on the class of the object
             # predict as a function thus becomes variable in it's output and 
             # arguments that it can take. 
             # That is why, it has to be customised for each kind of classifier
             # Here the best way to output the predictions is as class predictions
             # if you use type='vector', the output differs and is not a factor
             # This doesn't work for confusion matrix especially for LOTO as only 
             # 1 prediction is made at a time
             predicted <- predict(fit,testDataFold[,featureColNames],type='class')
             truth <- testDataFold[,classCol]
             Foldacc[i] <- sum(1 * (predicted==truth))/length(predicted)
             
             predictedTest <- predict(fit,ModelTestData[,featureColNames],type='class')
             truthTest <- ModelTestData[,classCol]
             FoldaccTest[i] <- sum(1 * (predictedTest==ModelTestData[,classCol]))/length(predictedTest)
             ConfMatrix[[i]] <- confusionMatrix(truthTest,predictedTest)
             
             
           }
           fit <- NULL
           accTest <- mean(FoldaccTest,na.rm=T)
           # no sense in getting an LDA model from a specific model
           if(!silent){
             #print(table(truth,predicted,dnn = c("Actual","Predicted")))
             print(paste("The CV fold accuracy of discrimination was",signif(mean(Foldacc,na.rm=T),2)))
             
             #print("Test Accuracies")
             #print(table(truthTest,predictedTest,dnn = c("Actual","Predicted")))
             print(paste("The Test accuracy of discrimination was",signif(mean(FoldaccTest,na.rm=T),2)))
             
             # print parameters used
             cat("k-fold CART Analysis:",'\nntrainTestFolds :', ntrainTestFolds,"\nnTrainFolds:",nTrainFolds,
                 '\nmodelTrainFolds',modelTrainFolds,"\nTest Accuracy",signif(mean(accTest),2))
             cat("\n*Legend:\nntrainTestFolds = No. of folds for training and testing dataset",
                 "\nnTrainFolds = No. of folds in which to further divide Training dataset",
                 "\nmodelTrainFolds = Specific folds from the above ntrainTestFolds to use for training",
                 "\nTest Accuracy = Mean accuracy from the Testing dataset\n")
             
             
           }
           
         },
         LOSO = {
           if(!silent) cat("\nPerforming Leave-one-subject-out Cross-validation \n\n")
           if(missing(foldSep)) stop("No foldSep provided")
           # get feature columns without response and Subject column (otherwise it will be used as a feature)
           featureColNames <- selectedColNames[!selectedColNames %in% c(names(Data)[classCol],names(Data[foldSep]))]
           
           Subs <- unique(Data[,foldSep])
           accTest <- rep(NA,length(Subs))
           
           ConfMatrix <- list()
           
           for (i in 1:length(Subs)){
             trainData <- Data[Data[,foldSep]!=Subs[i],]
             testData <- Data[Data[,foldSep]==Subs[i],]
             
             # CV = FALSE as we don't want leave-one-trial out cv
             fit <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=trainData[,selectedCols],method = 'class')
             
             # The predict funcion redirects based on the class of the object
             # predict as a function thus becomes variable in it's output and 
             # arguments that it can take. 
             # That is why, it has to be customised for each kind of classifier
             # Here the best way to output the predictions is as class predictions
             # if you use type='vector', the output differs and is not a factor
             # This doesn't work for confusion matrix especially for LOTO as only 
             # 1 prediction is made at a time
             predictedTest <- predict(fit,testData[,featureColNames],type='class')
             
             truthTest <- testData[,classCol]
             accTest[i] <- sum(1 * (predictedTest==testData[,classCol]))/length(predictedTest)
             ConfMatrix[[i]] <- confusionMatrix(truthTest,predictedTest)
             
             if(!silent){
               print(table(truthTest,predictedTest,dnn = c("Actual","Predicted")))
               print(paste("The accuracy of discrimination was",signif(mean(accTest,na.rm=T),2)))
             }
           }
           # no sense in getting model from cross-validation
           fit <- NULL
           accTest <- mean(accTest,na.rm=T)
           if(!silent){
             # print parameters used
             cat("leave-one-subject-out CART Analysis:",'\nnSubs :', length(Subs),"\nTest Accuracy",signif(accTest,2))
             cat("\n*Legend:\nnSubs = No. of Subjects in the data",
                 "\nTest Accuracy = Mean accuracy from the Testing dataset\n")
           }
         },
         holdout = {
           
           if(!silent) cat("\nPerforming holdout Cross-validation \n\n")
           
           if(missing(cvFraction)){
             if(!silent) cat("cvFraction was not specified,\n Using default value of 0.8 (cvFraction = 0.8)\n")
             cvFraction = 0.8
           }
           
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           index <- createDataPartition(Data[,classCol],p=cvFraction,times=1)
           trainData <- Data[1:nrow(Data) %in% index$Resample1,]
           testData <- Data[!(1:nrow(Data) %in% index$Resample1),]
           cat("Proportion of Test/Train Data was : ",nrow(testData)/nrow(trainData),"\n")
           
           fit <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=trainData[,selectedCols],method = 'class')
           
           # The predict funcion redirects based on the class of the object
           # predict as a function thus becomes variable in it's output and 
           # arguments that it can take. 
           # That is why, it has to be customised for each kind of classifier
           # Here the best way to output the predictions is as class predictions
           # if you use type='vector', the output differs and is not a factor
           # This doesn't work for confusion matrix especially for LOTO as only 
           # 1 prediction is made at a time
           predictedTest <- predict(fit,testData[,featureColNames],type='class')
           truthTest <- testData[,classCol]
           accTest <- sum(1 * (predictedTest==testData[,classCol]))/length(predictedTest)
           ConfMatrix <- confusionMatrix(truthTest,predictedTest)
           
           if(!silent){
             print(paste("Test holdout Accuracy is ",signif(accTest,2)))
             cat("holdout CART Analysis:",'\ncvFraction :', cvFraction,"\nTest Accuracy",signif(mean(accTest),2))
             
             cat("\n*Legend:\ncvFraction = Fraction of data to keep for training data",
                 "\nTest Accuracy = Accuracy from the Testing dataset\n")
           }
           
           # printing the tree makes sense only in case of holdout as in all other cases, there are multiple models
           # that generate the accuracy
           cp = fit$cptable[which(fit$cptable[,'xerror']==min(fit$cptable[,'xerror'])),'CP']
           prunedModel <- prune(fit,cp=cp)
           
           plot(prunedModel, uniform=TRUE,
                main="Pruned Classification Tree (without Missing)")
           text(prunedModel, use.n=TRUE, all=TRUE, cex=.8)
           if(!silent) print(prunedModel)
           
         },
         LOTO = {
           if(!silent) cat("\nPerforming Leave-one-Trial-out Cross-validation \n (Might take some time depending upon the size of dataset)  \n")
           featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
           
           index <- createFolds(Data[,classCol],k=nrow(Data),list=FALSE)
           accTest <- vector()
           ConfMatrix <- list()
           for(i in seq_along(index)){
             trainData <- Data[-i,]
             testData <-  Data[i,]
             
             fit <- rpart(as.formula(paste(responseColName,"~",paste0(featureColNames,collapse = "+"))),data=trainData[,selectedCols],method = 'class')
             
             # The predict funcion redirects based on the class of the object
             # predict as a function thus becomes variable in it's output and 
             # arguments that it can take. 
             # That is why, it has to be customised for each kind of classifier
             # Here the best way to output the predictions is as class predictions
             # if you use type='vector', the output differs and is not a factor
             # This doesn't work for confusion matrix especially for LOTO as only 
             # 1 prediction is made at a time
             predictedTest <- predict(fit,testData[,featureColNames],type='class')
             
             truthTest <- testData[,classCol]
             accTest[i] <- sum(1 * (predictedTest==testData[,classCol]))/length(predictedTest)
             ConfMatrix[[i]] <- confusionMatrix(truthTest,predictedTest)
             
             
           }
           # no sense in getting model from cross-validation
           fit <- NULL
           accTest <- mean(accTest)
           
           if(!silent){
             print(paste("Test LOTO classification Accuracy is ",signif(accTest,2)))
             # print parameters used
             cat("leave-one-Trial-out CART Analysis:","\nTest Accuracy",signif(accTest,2))
             cat("\n*Legend:\nTest Accuracy = Mean accuracy from the Testing dataset\n")
           }
           
         }
  )
  # get overall confusion Matrix results
  ConfusionMatrixResults <- overallConfusionMetrics(ConfMatrix)
  
  if(!is.null(NewData)){
    newDataprediction <- predictNewData(fit,NewData,type='class')
    fit <- list(fit=fit,newDataprediction=newDataprediction)
  }
  
  
  if(extendedResults){
    Results <- list(accTest = accTest,fit = fit,ConfMatrix=ConfMatrix,ConfusionMatrixResults=ConfusionMatrixResults)
    return(Results)
  }else return(accTest)

  
  
  
}
