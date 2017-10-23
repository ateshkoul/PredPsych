#' Permutation Analysis for classification
#'
#' simple function to create permutation testing of a classifier
#' 
#' @param Data            (dataframe) dataframe of the data
#' @param classCol        (numeric) column number that contains the variable to be predicted
#' @param selectedCols    (optional) (numeric) all the columns of data that would be used either as predictor or as feature
#' @param classifierFun   (optional) (function) classifier function
#' @param nSims           (optional) (numeric) number of simulations
#' @param plot            (optional) (logical) whether to plot null accuracy distribution
#' @param silent             (optional) (logical) whether to print messages or not
#' @param ...             (optional) additional arguments for the function
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
#' @examples
#' # perform a permutation testing for 10% of the kinematics movement data#' 
#' # not run
#' # PermutationResult <- ClassPerm(Data = KinData, classCol = 1,
#' # selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112), nSims = 1000,cvType = "holdout")
#' # Output:
#' # Performing Permutation Analysis for Classification
#' #
#' # Performing Cross-validation
#' #
#' # Performing holdout Cross-validation 
#' # genclassifier was not specified, 
#' #  Using default value of Classifier.svm (genclassifier = Classifier.svm)
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
#' # 
#' # Performing permutation testing...
#' # Performing 1000 simulations 
#' # |=======================================================
#' # ==================================================================|100%
#' #                      Completed after 2 m 
#' # The p-value of the permutation testing is 0.001
#' # p-value generated using the approximate method for p-value calculation. 
#' # See Phipson, B. & Gordon K., S. (2010) for details
#' 
#' 
#' # Using LinearDA instead as function
#' # not run
#' # PermutationResult <- ClassPerm(Data = KinData, classCol = 1,
#' # selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112), nSims = 1000,classifierFun = LinearDA)
#' 
#' 
#' # Any minimalistic function can be used 
#' # The ClassPerm function sends the dataframe Data, classCol, 
#' # selectedCols as arguments
#' # not run
#' # myMinimalFun <- function(...){
#' # ***Calculate Error function as you want***
#' # return(accTest)
#' # } 
#' # Use the function for permutation testing e.g.
#' # Results <- ClassPerm(Data = KinData, classCol=1,
#' # selectedCols = c(1,2,12,22,32,42,52,62,72,82,92,102,112), 
#' # nSims = 1000,classifierFun = myMinimalFun)
#' 
#' 
#'
#'@import e1071 ggplot2 plyr caret statmod
#'
#'@author
#'Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#'
#'\email{atesh.koul@@iit.it}
#'
#'@references 
#'Phipson, B., & Smyth, G. K. (2010). Permutation P-values Should Never Be Zero: Calculating Exact P-values When Permutations Are Randomly Drawn. 
#'Statistical Applications in Genetics and Molecular Biology, 9(1), 1544-6115.
#'
#'Ojala, M. & Garriga, G. C. Permutation Tests for Studying Classifier Performance. J. Mach. Learn. Res. 11, 1833-1863 (2010).
#'
#'Good, P. (2005). Permutation, Parametric and Bootstrap Tests of Hypotheses. New York: Springer-Verlag.
#'
#' @export
ClassPerm <- function(Data,classCol,selectedCols,classifierFun,nSims=1000,plot=TRUE,silent=FALSE,...){
  # classifierFun is a function that the use inputs to calculate the permutation scores
  # The form of this function should return accuracy as a single value.
  # Extra options should be specified in the classifier function
  # Sample classifier function
  # load libraries
  # library(caret)
  # library(ggplot2)
  # library(plotly)
  # library(plyr)

  # a bit complicated to implement a generic way to feed in variable arguements
  # match variable input
  # extras <- match.call(expand.dots= F)$...
  if(!silent) cat("\nPerforming Permutation Analysis for Classification \n\n")
  
  # Modified so that it works even with tibble; force the tibble to be a dataframe
  # This is not the best way to proceed; Ideally, all the code should be updated 
  # to work with tibble
  if("tbl_df" %in% class(Data)) Data <- as.data.frame(Data)

  
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
  
  
  cat("Performing Cross-validation\n")
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
    # override possible given silent option 
    silent <- TRUE
    NullAcc <- classifierFun(Data,predictorCol,selectedCols,silent=TRUE,SetSeed = FALSE,...)
    return(NullAcc)
  }

  cat("\n \nPerforming permutation testing...\n")
  # default to 1000 repetitions
  if(!exists("nSims")) nSims <- 1000

  cat(paste0("\nPerforming ",nSims," simulations\n \n"))
  
  # important to set seed here not only for reproducibility
  # also so that we don't get same results over and over again if
  # we set seed in the classification function
  #set.seed(111)
  
  # to avoid the note in R CMD check
  nullAcc <- NULL
  distNull <- data.frame(nullAcc=unlist(rlply(nSims, permutator(Data,classCol,selectedCols),.progress = progress_time())))
  
  # calculate the number of times, the test statistic was greate than observed one
  B <- sum(distNull$nullAcc >= actualAcc)
  
  # A generic implementation of emperical p-values (defined by a Monte Carlo procedure)
  # by Ojala, M. & Garriga, G. C. (2010) as well as by Good P. (2000):
  # 
  # p_value = (sum(distNull$nullAcc >= actualAcc)+1)/(nSims+1)
  # 
   
  # This implementation however, might inflate the type 1 error as reported by:
  # Phipson, B. & Gordon K., S. (2010).
  # The current implementation follows recommendation by Phipson, B. & Gordon K., S.
  # and their package statmod for calculating either "exact" or "approximate" p-values
  
  # for higher sample sizes (as is the case with most of classification analysis),
  # approximate, instead of exact p-values are used. It's better to choose automatically
  # here whether exact or approximate values are used (based on computational power).
  # 
  # I utilize the log of the factorial as direct factorial values can get really high.
  # These high values would give you Inf. 
  # 
  # The procedure is also generic compared to that from permp that defaults total permutations
  # only for 2 classes.
  # 
  # generalise the permp function to use with more than 2 groups
  classSizes <- table(Data[,classCol])
  
  
  lPossibleComb <- lfactorial(sum(classSizes)) - sum(lfactorial(classSizes))
  
  # get the actual number of permutations to create
  possibleComb <- exp(lPossibleComb)
  
  # print the method used (for computational sanity)
  # equivalent to choose if (lpossibleComb>log(10000)) pMethod <- "approximate"
  # Same criteria used as in permp (check if using statmod version other than 1.4.29)
  pMethod <- ifelse(possibleComb>10000,"approximate","exact")
  
  p_value <- permp(B,nSims,total.nperm = possibleComb)
  
  cat(paste0("\nThe p-value of the permutation testing is ",signif(p_value,2)))
  
  cat(paste0("\n \np-value generated using the ",pMethod," method for p-value calculation. \nSee Phipson, B. & Gordon K., S. (2010) for details"))
  

  if(plot){
  # plot with automatically adjusting the height of the y-axis using 1 sd of the data
  plotPerm <- ggplot(distNull,aes(nullAcc))+
    #geom_line(aes(x = c(actualAcc,actualAcc),y=c(0,max(density(dframe$x)$y)+ sd(density(dframe$x)$y))),data=dataActual)+
    geom_vline(xintercept = actualAcc,colour='blue')+
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

  print(plotPerm)
  }
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
  #library(e1071)
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




