\name{BostonHousing}
\Rdversion{1.1}
\alias{BostonHousing}
\docType{data}
\title{
BostonHousing
}
\description{
      Housing data for 506 census tracts of Boston from the 1970 census.
     The dataframe 'BostonHousing' contains the original data by
     Harrison and Rubinfeld (1979), the dataframe 'BostonHousing2' the
     corrected version with additional spatial information (see
     references below).}
\usage{data(BostonHousing)}
\format{
     The original data are 506 observations on 14 variables, 'medv'
     being the target variable:
  \describe{
    \item{\code{crim}}{per capita crime rate by town}
    \item{\code{zn}}{proportion of residential land zoned for lots over 25,000 sq.ft}
    \item{\code{indus}}{proportion of non-retail business acres per town}
    \item{\code{chas}}{Charles River dummy variable (= \code{1} if tract bounds river; \code{0} otherwise)}
    \item{\code{nox}}{nitric oxides concentration (parts per 10 million)}
    \item{\code{rm}}{average number of rooms per dwelling}
    \item{\code{age}}{proportion of owner-occupied units built prior to 1940}
    \item{\code{dis}}{weighted distances to five Boston employment centres}
    \item{\code{rad}}{index of accessibility to radial highways}
    \item{\code{tax}}{full-value property-tax rate per USD 10,000}
    \item{\code{ptratio}}{pupil-teacher ratio by town}
    \item{\code{b}}{1000(B - 0.63)^2 where B is the proportion of blacks by town}
    \item{\code{lstat}}{percentage of lower status of the population}
    \item{\code{medv}}{median value of owner-occupied homes in USD 1000's}
  }
}
\details{
    The original data have been taken from the UCI Repository Of
     Machine Learning Databases at

       \url{http://www.ics.uci.edu/~mlearn/MLRepository.html},

     See Statlib and references there for details on the corrections.
     Converted to R format by Friedrich Leisch.}
\references{
     Harrison, D. and Rubinfeld, D.L. (1978). Hedonic prices and the
     demand for clean air. _Journal of Environmental Economics and
     Management_, *5*, 81-102.

     Gilley, O.W., and R. Kelley Pace (1996). On the Harrison and
     Rubinfeld   Data. _Journal of Environmental Economics and
     Management_, *31*,   403-405. [Provided corrections and examined
     censoring.]

     Newman, D.J. & Hettich, S. & Blake, C.L. & Merz, C.J. (1998). UCI
     Repository of machine learning databases
     [http://www.ics.uci.edu/~mlearn/MLRepository.html]. Irvine, CA:
     University of California, Department of Information and Computer
     Science.

     Pace, R. Kelley, and O.W. Gilley (1997). Using the Spatial
     Configuration of   the Data to Improve Estimation. _Journal of the
     Real Estate Finance   and Economics_, *14*, 333-340. [Added
     georeferencing and spatial estimation.]}
\keyword{datasets}
