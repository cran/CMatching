\name{summary.CMatch}
\alias{summary.CMatch}
\alias{print.summary.CMatch}
\title{
Summarizing output from MatchW and MatchPW functions
}
\description{
Summary method for \code{\link{MatchW}} and \code{\link{MatchPW}}
}
\usage{
\method{summary}{CMatch}(object, ..., full = FALSE, digits = 5)
}
\arguments{
  \item{object}{
An object of class "\code{CMatch}".
}
  \item{\dots}{
Other options for the generic summary function.
}
  \item{full}{
A flag for whether the unadjusted estimates and naive standard errors should also be summarized.
}
  \item{digits}{
The number of significant digits that should be displayed.
}
}
\details{
If \code{Group} contains only one value the output is the same of the summary method of package \code{Matching}. Otherwise the output shows also the distribution of treated, control and possibly drop units, \emph{by group}.
}
\value{
A list giving a summary of the output from a "\code{CMatch}" object. The list includes the size of the original and the matched dataset, the number of treated and control observations in each group and the estimate (if \code{Y} is not \code{NULL}). 
}

\references{
Sekhon, Jasjeet S. 2011. Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/

Arpino, B., and Cannas, M. (2016) Propensity score matching with clustered data. An application to the estimation of the impact of caesarean section on the Apgar score. \emph{Statistics in Medicine, 35: 2074–2091}. doi: 10.1002/sim.6880.
}
\author{
Massimo Cannas  <massimo.cannas@unica.it>
}
\note{
Naive standard errors are not available when there is more than one group so the \code{full} parameter is ineffective in that case. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link[CMatching]{CMatch}}, \code{ \link[CMatching]{CMatchBalance}}
}
