% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{modelPrediction}
\alias{modelPrediction}
\title{This function is to predict outcomes of test data of single fitted model.}
\usage{
modelPrediction(X, y, ntree = 500, p = 0.8)
}
\arguments{
\item{X}{A matrix of predictor variables}

\item{y}{A vector of response variable}

\item{ntree}{The number of trees to grow. This parameter is used to predict outcome on random frest model.}

\item{p}{The proportion of train data}
}
\value{
If y is binary, it returns a list of
1. confusion.matrix: A confusion matrix between predicted data and test data
2. accuracy: An accuracy of prediction
If y is continuous, it returns
RMSE: RMSE(Root Mean Square Error) between predicted data and test data.
}
\description{
This function is to predict outcomes of test data of single fitted model.
}
