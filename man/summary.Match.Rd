\name{summary.Match}
\alias{summary.Match}
\alias{print.summary.Match}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing output from MatchW and MatchPW
}
\description{
summary method for \code{\link{MatchW}} and \code{\link{MatchPW}}
}
\usage{
\method{summary}{Match}(object, ..., full = FALSE, digits = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class "\code{Match}".
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
A summary of most important output from a "\code{Match}" object. If \emph{Group} contains only one value the output is the same of \code{summary(Match())}. Otherwise the output contains also the distribution of treated (control) observations by group and the distribution of dropped (because od 'caliper' or 'exact' option) by group. 
}
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
\references{
Sekhon, Jasjeet S. 2011. Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/

Arpino, B., and Cannas, M. (2016) Propensity score matching with clustered data. An application to the estimation of the impact of caesarean section on the Apgar score. \emph{Statist. Med., 35: 2074–2091}. doi: 10.1002/sim.6880.
}
\author{
Massimo Cannas  <massimo.cannas@unica.it>
}
\note{
Naive standard errors are not available when there is more than one group so the \code{full} parameter is ineffective in that case. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{Match}},\code{\link{MatchW}},\code{\link{MatchPW}}, \code{\link{MatchBalance}}
}
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%}
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
