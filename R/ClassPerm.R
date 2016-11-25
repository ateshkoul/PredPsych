#' Permutation Analysis for classification
#'
#' simple function to create permutation testing of a classifier
#' 
#' @param Data            (dataframe) dataframe of the data
#' @param classCol        (numeric) column number that contains the variable to be predicted
#' @param selectedCols    (optional) (numeric) all the columns of data that would be used either as predictor or as feature
#' @param classifierFun   (optional) (function) classifier function
#' @param nSims           (optional) (numeric) number of simulations
#'
#' @details 
#' The function implements Permutation tests for classification.
#' Permutation tests are a set of non-parametric methods for hypothesis testing without assuming 
#' a particular distribution (Good, 2005). In case of classification analysis, this requires 
#' shuffling the labels of the dataset (i.e. randomly shuffling classes/conditions between observations)
#'   and calculating accuracies obtained.
#'
#' @return Returns \code{actualAcc} of the classification analysis,
#'  \code{p-value} from permutation testing, \code{nullAcc} distribution of the permutation \code{figure} containing null distribution
#'
#'@examples
#'# perform a permutation testing for 10% of the kinematics movement data
#'PermutationResult <- ClassPerm(Data = KinData, classCol = 1,
#'  selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112), nSims = 1000)
#'
#'
#'
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#' @export
ClassPerm <- function(Data,classCol,selectedCols,classifierFun,nSims=1000,...){
  # classifierFun is a function that the use inputs to calculate the permutation scores
  # The form of this function should return accuracy as a single value.
  # Extra options should be specified in the classifier function
  # Sample classifier function
  # load libraries
  library(caret)
  library(ggplot2)
  library(plotly)
  library(plyr)

  # a bit complicated to implement a generic way to feed in variable arguements
  # match variable input
  # extras <- match.call(expand.dots= F)$...
  if(missing(selectedCols))  selectedCols <- 1:length(names(Data))
  
  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-grep(names(Data)[classCol],selectedColNames)]
  
  # X <- Data[,featureColNames]
  # Y <- Data[predictor]
  
  if (missing(classifierFun)){
    
    # if missing, use the default function of classifyFun from the library with default of svm
    classifierFun <- classifyFun
  }
  # else {
  #   print(classifierFun)
  #   classifierFun <- get(classifierFun)
  # }

  # if your targets are not factors, make them..
  if(!(is.factor(Data[,classCol]))) Data[,classCol] <- factor(Data[,classCol])
  
  
  print("Performing Cross Validation")
  set.seed(111)
  # First calculate actual accuracy
  # Keep the seed constant in this case (we want the same accuracy)
  actualAcc <- classifierFun(Data,classCol,selectedCols,SetSeed = FALSE,...)
  # calculate permutation scores by randomly sampling targets
  chanceAcc <- 1/(length(unique(Data[,classCol])))
  # permutator is a simple function that randomly shuffles targets and spits out accuracies
  permutator <-function(Data,predictorCol,selectedCols){
    Data[,predictorCol] <- sample(Data[,predictorCol])
    # use silence to not print the accuracies multiple times
    # change the seed; otherwise the sample function above always outputs the same value
    NullAcc <- classifierFun(Data,predictorCol,selectedCols,silent=TRUE,SetSeed = FALSE,...)
    return(NullAcc)
  }

  print("Performing permutation testing...")
  # default to 1000 repetitions
  if(!exists("nSims")) nSims <- 1000

  print(paste0('performing ',nSims,' simulations'))
  # important to set seed here not only for reproducibility
  # also so that we don't get same results over and over again if
  # we set seed in the classification function
  #set.seed(111)

  distNull <- data.frame(nullAcc=unlist(rlply(nSims, permutator(Data,classCol,selectedCols),.progress = progress_time())))
  p_value = sum(distNull$nullAcc >= actualAcc)/nSims
  print(paste0('The p-value of the permutation testing is ',p_value))

  # plot with automatically adjusting the height of the y-axis using 1 sd of the data
  plot <- ggplot(distNull,aes(nullAcc))+
    #geom_line(aes(x = c(actualAcc,actualAcc),y=c(0,max(density(dframe$x)$y)+ sd(density(dframe$x)$y))),data=dataActual)+
    geom_vline(xintercept = actualAcc,colour='red')+
    geom_vline(xintercept = chanceAcc,colour='red')+
    geom_density(fill='darkblue',alpha=0.3)+
    ggtitle("Permutation curve") +
#     geom_curve(x = actualAcc-0.18, xend = actualAcc-0.01, y = max(density(distNull$nullAcc)$y)-0.03,
#                yend = max(density(distNull$nullAcc)$y)-0.5,curvature = 0.2,
#                arrow = grid::arrow(length = grid::unit(0.03, "npc")))+
    #   annotate("text", x = actualAcc-0.18,y = max(density(distNull$nullAcc)$y)+0.1,
    #                       colour="blue", label = paste0('Actual Accuracy ',as.character(signif(actualAcc,2))),size=5)+
    annotate("text", x = actualAcc-0.03,y = 0.2,
             colour="blue", label = as.character(signif(actualAcc,2)),size=4)+
    annotate("text", x = chanceAcc-0.03,y = 0.2,
             colour="blue", label = 'chance',size=4)+
    theme_bw(base_size = 18)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line.y = element_line(color = 'black'),
          axis.line.x = element_line(color = 'black'))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(density(distNull$nullAcc)$y)+ 0.3*sd(density(distNull$nullAcc)$y))) +
    scale_x_continuous(expand = c(0.01, 0.01), limits = c(-0.1, 1.01))

  print(ggplotly(plot))
  Results = list(actualAcc = actualAcc,p_value=p_value,nullAcc = distNull$nullAcc,plot=plot,distNull= distNull)
  return(Results)

}


# LinearDAPerm <- function(X,Y,cvType="LOTO"){
#   #simple function to perform linear discriminant analysis
#   # DOn't set seed here as it goes back to the permutator function and constraints the sample 
#   # to set the same value for Y over and over again.
#   library(MASS)
#   library(caret)
#   if(cvType=="LOTO"){
#     index <- createFolds(Y,k=length(Y),list=FALSE)
#     acc <- vector()
#     
#     for(i in seq_along(index)){
#       XTrain <- X[-i,]
#       YTrain <- Y[-i]
#       XTest <-  X[i,]
#       YTest <-  Y[i]
#       fit <- lda(XTrain,grouping = YTrain)
#       predicted <- predict(fit,newdata=XTest)
#       #print(table(predicted$class,DataTest[,predictorCol]))
#       acc[i] <- sum(1 * (predicted$class==YTest))/length(predicted$class)
#     }
#     Acc <- mean(acc)
#     #print(paste("The accuracy of discrimination was",signif(Acc,2)))
#   }  
#   
#   return(Acc)
#   
#   
# }


LinearSVM <- function(Data,classCol,selectedCols,SetSeed = T,silent,...){
  # a simplistic k-fold crossvalidation
  # For cross validation
  library(e1071)
  #set.seed(111)
  # defaults to 10 fold cross validation
  selectedColNames <- names(Data)[selectedCols]
  # get feature columns without response
  featureColNames <- selectedColNames[-match(names(Data)[classCol],selectedColNames)]
  predictorColNames <- names(Data)[classCol]
  
  Data = Data[,selectedCols]
  Data[,predictorColNames] = factor(Data[,predictorColNames])
  
  k = 10
  # use stratified cross validation instead
  # use 80% data for training
  trainIndex <- createFolds(Data[,classCol], list = FALSE,k = k)
  acc <- rep(NA,k)
  for (i in 1:k){
    trainX <- Data[!trainIndex==i,featureColNames]
    testX <- Data[trainIndex==i,featureColNames]
    trainY <- Data[!trainIndex==i,predictorColNames]
    testY <- Data[trainIndex==i,predictorColNames]
    model <- svm(trainX, trainY,kernel = "linear")
    # test with train data
    pred <- predict(model, testX)
    acc[i] <- sum(1 * (pred==testY))/length(pred)
  }
  
  # old Method
  
  #       folds <- cvFolds(nrow(X),K = k)
  #       acc <- rep(NA,k)
  #
  #       for (i in 1:k){
  #         trainX <- X[folds$subsets[folds$which != i], ]
  #         testX <- X[folds$subsets[folds$which == i], ]
  #         trainY <- Y[folds$subsets[folds$which != i]]
  #         testY <- Y[folds$subsets[folds$which == i]]
  #         model <- svm(trainX, trainY,kernel = "linear")
  #         # test with train data
  #         pred <- predict(model, testX)
  #         acc[i] <- sum(1 * (pred==testY))/length(pred)
  #       }
  return(mean(acc,na.rm=T))
}