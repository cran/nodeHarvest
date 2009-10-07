\name{predict.nodeHarvest}
\Rdversion{1.1}
\alias{predict.nodeHarvest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Node Harvest prediction for new observations
}
\description{
Given new observations, compute the prediction of a node harvest estimator.
}
\usage{
\method{predict}{nodeHarvest}(object, newdata = NULL, explain = 1, 
                maxshow = 5, 
                weight = sapply(object[["nodes"]], attr, "weight"), ...)
}
\arguments{
  \item{object}{
An object of class \code{nodeHarvest}.
}
  \item{newdata}{
A data matrix with predictor variables. If missing, the predictions on the training data are returned.
}
 \item{explain}{
Row numbers for \code{newdata} for which the predictions should be `explained'. 
If \code{NULL}, no explanation is given.
}
 \item{maxshow}{
When explaining a prediction, show at most this many nodes (the most important ones). 
}
  \item{weight}{
Optional changed weight vector for the nodes.
}
\item{...}{
Additional arguments passed to \code{predict}
}
}
\details{
If \code{explains} is equal to \code{NULL}, no output is printed. 
If \code{explain} is a numeric vector (with values in 1 to the number of samples in \code{newdata}), for each observation in \code{newdata} with a sample number in vector \code{explain}, the following is done:
all nodes that the observation belongs to are printed on screen, along with their node mean (the mean of all training observations who fell into this node) and weight. The prediction for this new observation is the weighted average across these node means. The number of nodes show is given in descending order of their importance (weight) and the number of nodes shown is limited to \code{maxshow}.
}

\value{
A numeric vector with the predicted response.
}

\author{
Nicolai Meinshausen
\email{meinshausen@stats.ox.ac.uk} 

\url{http://www.stats.ox.ac.uk/~meinshau}}


\seealso{
\code{\link{nodeHarvest}}, 
\code{\link{plot.nodeHarvest}}
}

