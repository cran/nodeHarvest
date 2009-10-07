\name{nodeHarvest}
\Rdversion{1.1}
\alias{nodeHarvest}
\title{ Node Harvest }
\description{
Computes the node harvest estimator
}
\usage{
nodeHarvest(X, Y, nodesize = 10, 
	       	  nodes = 1000, 
		  maxinter = 1, 
		  lambda = Inf, 
		  addto = NULL, 
		  onlyinter = NULL,
                  silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  A n x p - dimensional data matrix, where n is sample size and p is the dimensionality of the predictor variable.
  Factorial variables are currently converted to numerical variables (will be changed in the future).
  Missing values are supported. 
  }
  \item{Y}{
  A numerical vector of length n, containing the observations of the response variable. 
  Can be continuous (regression) or binary 0/1 (classification).
}
  \item{nodesize}{
  Minimal number of samples in each node.
}
  \item{nodes}{
  Number of nodes in the initial large ensemble of nodes.
}
  \item{maxinter}{
  Maximal interaction depth 
  (0 = only main effects; 1 = first order interactions etc).
}
  \item{lambda}{
  Optional upper bound on the inverse of the average weighted fraction of samples within each node.
}
  \item{addto}{
  A previous node harvest estimator to which additional nodes should be attached
  (useful for iterative growth of the estimator when hitting memory constraints).
}
  \item{onlyinter}{
  Allow interactions only for this list of variables.
}
\item{silent}{
If \code{TRUE}, no comments are printed.
}
}
\details{
  The number of nodes should be chosen as large as possible under the available computational resources.
  If these resources are limited, an estimator can be build by iteratively calling the function, adding the previous 
  estimator via the \code{addto} argument.

  Feedback and feature requests are more than welcome (email below).
}
\value{
A list with entries
  \item{nodes      }{A list of all selected nodes}
  \item{predicted  }{Predicted values on training data}
  \item{connection }{Connectivity matrix between selected nodes (used for plotting)}
  \item{varnames   }{Variable names} 
  \item{Y          }{The original observations} 
}
\references{
`Node harvest: simple and interpretable regression and classification' (arxiv:0910.2145v1)

 \url{http://arxiv.org/abs/0910.2145}
}
\author{
Nicolai Meinshausen 
\email{meinshausen@stats.ox.ac.uk} 

\url{http://www.stats.ox.ac.uk/~meinshau}}


\seealso{
\code{\link{predict.nodeHarvest}}, 
\code{\link{plot.nodeHarvest}}
}
\examples{

## Load Boston Housing dataset
    library(mlbench)
    data(BostonHousing)
    X <- BostonHousing[,1:13]
    Y <- BostonHousing[,14]

## Divide data into training and test data
    n <- nrow(X)
    training <- sample(1:n,round(n/2))
    testing <- (1:n)[-training]

    
## Train Node Harvest and plot and print the estimator
    NH <- nodeHarvest( X[training,], Y[training], nodes=500 )
    plot(NH)
    print(NH, nonodes=5)	
    
## Predict on test data and explain prediction of the first sample in the test set
    predicttest <- predict(NH, X[testing,], explain=1)
    plot( predicttest, Y[testing] )

}
