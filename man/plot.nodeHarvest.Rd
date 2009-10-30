\name{plot.nodeHarvest}
\Rdversion{1.1}
\alias{plot.nodeHarvest}
\title{
plot method for Node Harvest objects
}
\description{
Node Harvest visualization. Each node with a non-zero weight is plotted (weights is proportional to area of node).
The average response in each node is shown on the horizontal axis and the sample size in each node on the vertical axis.
}
\usage{
\method{plot}{nodeHarvest}(x,  XTEST = NULL, 
	 highlight = NULL, 
         varnames = NULL, 
         threshold = 0.005, 
	 yoffset = 0.12, 
	 labels = "all", 
         cexfaclab = 1,  ...)
}
\arguments{
  \item{x}{
An object of class \code{nodeHarvest}.
}
  \item{XTEST}{
New observations (for highlighting relevant nodes).
}
  \item{highlight}{
The nodes of this observation in \code{X} are highlighted and possibly annotated, depending on argument \code{labels}.
}
  \item{varnames}{
The variable names can be changed here.
}
  \item{threshold}{
Nodes with weight below this value are not plotted.
}
  \item{yoffset}{
The vertical offset in the annotation of interaction nodes.
}
  \item{labels}{
 If `none', no annotation is made. If `all', all nodes are annotated. Otherwise the nodes given in highlight are annotated only.
}
  \item{cexfaclab}{
Character expansion factor for node annotation.
}
\item{...}{
Additional arguments passed to \code{plot}.
}
}

\value{
None.
}

\author{
Nicolai Meinshausen \email{meinshausen@stats.ox.ac.uk} 

\url{http://www.stats.ox.ac.uk/~meinshau}
}


\seealso{
\code{\link{nodeHarvest}}, 
\code{\link{plot.nodeHarvest}}
}

