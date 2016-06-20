#'simple function to create permutation testing of a classifier
#'
#' Needs the following:
#' :
##' \enumerate{
##' \item Inputs
##'   \enumerate{
##'     \item X = Data matrix
#'      \item Y = labels
#'      \item classifierFun = classifier function
#'      \item nSims = number of simulations
#'
#'  \enumerate{
##' \item Outputs
##'   \enumerate{
##'     \item p-value from permutation
##'     \item figure containing null distribution
#'
#'
#'@author
#'Atesh Koul, RBCS, Istituto Italiano di technologia
#'
#'\email{atesh.koul@@gmail.com}
ClassPerm <- function(X,Y,classifierFun,nSims=1000,...){
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



  if (missing(classifierFun)){
    print("Performing Cross Validation")
    classifierFun <- function(X,Y){
      # a simplistic k-fold crossvalidation
      # For cross validation
      library(e1071)
      #set.seed(111)
      # defaults to 10 fold cross validation
      k = 10
      # use stratified cross validation instead
      # use 80% data for training
      trainIndex <- createFolds(Y, list = FALSE,k = k)
      acc <- rep(NA,k)
      for (i in 1:k){
        trainX <- X[!trainIndex==i,]
        testX <- X[trainIndex==i,]
        trainY <- Y[!trainIndex==i]
        testY <- Y[trainIndex==i]
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
  }

  # if your targets are not factors, make them..
  if(!(is.factor(Y))) Y <- factor(Y)

  # First calculate actual accuracy
  actualAcc <- classifierFun(X,Y)
  # calculate permutation scores by randomly sampling targets
  chanceAcc <- 1/(length(unique(Y)))
  # permutator is a simple function that randomly shuffles targets and spits out accuracies
  permutator <-function(X,Y){
    Y <- sample(Y)
    NullAcc <- classifierFun(X,Y)
    return(NullAcc)
  }

  print("Performing permutation testing...")
  # default to 1000 repetitions
  if(!exists("nSims")) nSims <- 1000

  print(paste0('performing ',nSims,' simulations'))
  # important to set seed here not only for reproducibility
  # also so that we don't get same results over and over again if
  # we set seed in the classification function
  set.seed(111)

  distNull <- data.frame(nullAcc=unlist(rlply(nSims, permutator(X,Y),.progress = progress_time())))
  p_value = sum(distNull$nullAcc >= actualAcc)

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


