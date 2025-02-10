\name{CMatchBalance}
\alias{CMatchBalance}
\alias{CMatchBalance.default}
\alias{CMatchBalance.CMatch}
\title{
Analyze covariate balance before and after matching.
}
\description{
Generic function for analyzing covariate balance. If \code{match.out} is \code{NULL} only balance statistics for the unmatched data are returned otherwise both before and after matching balance are given. The function is a wrapper calling \code{MatchBalance}, possibly after coercing the class of \code{match.out}. See \code{MatchBalance} for more detailed description.
}
\usage{
CMatchBalance(match.out, formula, data = NULL, ks = TRUE, 
nboots = 500, weights = NULL, digits = 5, paired = TRUE, print.level = 1)
}
\arguments{
  \item{match.out}{
A matched data set, i.e., the result of a call to \code{Match} or \code{CMatch}. 
}
  \item{formula}{
This formula does not estimate a model. It is a compact way to describe which variables should be compared between the treated and control group. See \code{MatchBalance}. 
}
  \item{data}{
An optional data set for the variables indicated in the \code{formula} argument.
}
  \item{ks}{
A flag for whether Kolmogorov-Smirnov tests should be calculated.
}
  \item{weights}{
A vector of observation-specific weights.
}
  \item{nboots}{
The number of bootstrap replication to be used.
}
  \item{digits}{
The number of digits to be displayed in the output
}
  \item{paired}{
A flag for whether a paired t.test should be used for the matched data. An unpaired t.test is always used for unmatched data.
}
  \item{print.level}{
The amount of printing, taking values 0 (no printing), 1(summary) and 2 (dtailed results). Default to 1.
}
  % \item{\dots}{
  	% Other arguments to be passed to the \code{MatchBalance} function.
% %%     ~~Describe \code{\dots} here~~
%}
}
\details{
The function is a wrapper of the \code{MatchBalance} function. If \code{match.out} is of class \code{Match} (or \code{NULL}) then it calls \code{MatchBalance}. If \code{match.out} is of class\code{CMatch} then it coerces the class to \code{Match} before calling \code{MatchBalance}. This function is meant to exploit \code{MatchBalance} for \code{CMatch} objects for which \code{MatchBalance} would not work. 
}
\value{
	Balance statistics for the covariates specified in the \emph{right} side of \code{formula} argument. Statistics are compared between the two groups specified by the binary variable in the \emph{left} side of \code{formula}.
}
\references{
%% ~put references to the literature/web site here ~
Sekhon, Jasjeet S. 2011. Multivariate and Propensity Score Matching Software with Automated Balance Optimization. \emph{Journal of Statistical Software 42(7): 1-52.} http://www.jstatsoft.org/v42/i07/
}
\author{
\packageMaintainer{CMatching} and a special thanks to Thomas W. Yee for his help.
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[Matching]{MatchBalance}}
}
\examples{

data(schools)
 
# Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).   
# The data set is the subsample of NELS-88 data consisting of 10 handpicked schools
# from the 1003 schools in the full data set.
 
# Let us consider the following variables:

X<-schools$ses  # (socio economic status) 
Y<-schools$math #(mathematics score)
Tr<-ifelse(schools$homework > 1, 1 ,0)
Group<-schools$schid #(school ID)

# Multivariate Matching on covariates X 

### Matching within schools
 mw <- CMatch(type="within",Y=Y, Tr=Tr, X=X, Group=Group, caliper=0.1)
 
 # Balance statistics for X variables(s) before and after matching within schools. 
 CMatchBalance(Tr~X,data=schools,match.out = mw)
 
 
### Match preferentially within school 
# i.e. first match within schools
# then tries to match remaining units between schools

 mpw <- CMatch(type="pwithin",Y=schools$math, Tr=Tr, X=schools$ses, 
 Group=schools$schid, caliper=0.1)

# examine covariate balance of variable(s) X before and after preferential matching within schools
  CMatchBalance(Tr~X, data=schools, match.out = mpw)

}
\keyword{ matching }
\keyword{ covariate balance }
