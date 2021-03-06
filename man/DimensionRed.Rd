% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DimReduction.R
\name{DimensionRed}
\alias{DimensionRed}
\title{Generic Dimensionallity Reduction Function}
\usage{
DimensionRed(Data, method = "MDS", selectedCols, outcome = NA,
  plot = FALSE, silent = FALSE, ...)
}
\arguments{
\item{Data}{(dataframe)  a data frame with variable/feature columns}

\item{method}{(optional) (character) Dimensionality reduction method to be used}

\item{selectedCols}{(optional)(numeric) which columns should be treated as data(features/columns) (defaults to all columns)}

\item{outcome}{(optional)(vector) optional vector for visualising plots}

\item{plot}{(optional)(logical) To plot or not to plot}

\item{silent}{(optional) (logical) whether to print messages or not}

\item{...}{(optional) additional arguments for the function}
}
\value{
Data frame with \code{Results}
}
\description{
A simple function to perform dimensionality reduction
}
\details{
Dimensionality Reduction is the process of reducing the dimensions of the dataset. 
Multivariate data, even though are useful in getting an overall understanding of the underlying phenomena,
do not permit easy interpretability. Moreover, variables in such data often are correlated with each other
.For these reasons, it might be imperative to reduce the dimensions of the data. 
Various models have been developed for such dimensionality reduction. Of these, MDS and PCA has been 
demonstrated in the current implementation.
}
\examples{
# reducing dimension of Grip aperture from 10 to 2
GripAperture <- DimensionRed(KinData,selectedCols = 12:21,
outcome = KinData[,"Object.Size"],plot = TRUE)

}
\author{
Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia

\email{atesh.koul@iit.it}
}
\references{
Bishop, C. M. (2006). Pattern Recognition and Machine Learning. (M. Jordan, J. Kleinberg, & B. Scholkopf, Eds.) 
(1st ed.). Springer-Verlag New York.

Cox, T. F., & Cox, M. A. A. (2000). Multidimensional scaling (Second ed.). Chapman & Hall/CRC.
}

